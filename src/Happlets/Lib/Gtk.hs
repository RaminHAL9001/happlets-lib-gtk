{-# LANGUAGE CPP #-}

-- | This is the Gtk+ version 2 back-end for Happlets, and will also serve as the Happlet referecne
-- implementation. Creating a window automatically calls the the Gtk+ initializer. Use
-- 'newGtkWindow' to create a new window, and any of the "Happlet.World" functinos to manipulate the
-- windows. This module re-exports the "Happlets" module so it is not necessary to import both.
module Happlets.Lib.Gtk
  ( gtkHapplet, gtkAnimationFrameRate, GtkGUI,
    GtkWindow, gtkLaunchEventLoop,
    GtkImage,
    -- * Cairo Wrapper Functions
    -- | Functions that allow you to call directly into a "Graphics.Rendering.Cairo".'Cairo.Render'
    -- function, but using point, line, and color values specified in the "Happlets.Draw"
    -- sub-modules.
    CairoRender, cairoRender, GtkCairoDiagram, gtkCairoDiagram,
    cairoClearCanvas, cairoSetColor,
    cairoDrawPath, cairoMoveTo, cairoLineTo, cairoDrawLine,
    cairoDrawRect, cairoPreserve,
    cairoFlush, cairoSetPoint, cairoGetPoint, cairoInvalidate,
    module Happlets,
    module Happlets.Draw
  )
  where

-- I have found that it is probably better to do double-buffering of the canvas by using a Cairo
-- Surface, however there is a way to make use of a Gtk+ Pixmap as a double-buffer instead. Both
-- ways have been encoded in this source file, because I am not sure which is really the most
-- portable, or which is the most efficient.
#define USE_CAIRO_SURFACE_BUFFER 1

----------------------------------------------------------------------------------------------------

import           Happlets
import           Happlets.Draw
import           Happlets.Provider

import           Control.Arrow
import           Control.Concurrent

import           Data.Array.MArray
import           Data.Bits
import           Data.IORef
import qualified Data.Map          as Map
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Text         as Strict
import           Data.Time.Clock
import           Data.Word

import qualified Graphics.Rendering.Cairo           as Cairo

import qualified Graphics.UI.Gtk.Abstract.Container as Gtk
import qualified Graphics.UI.Gtk.Abstract.Widget    as Gtk
import qualified Graphics.UI.Gtk.Cairo              as Gtk
import qualified Graphics.UI.Gtk.Gdk.Drawable       as Gtk
import qualified Graphics.UI.Gtk.Gdk.DrawWindow     as Gtk
import qualified Graphics.UI.Gtk.Gdk.EventM         as Gtk
import qualified Graphics.UI.Gtk.Gdk.Keys           as Gtk
import qualified Graphics.UI.Gtk.Gdk.Region         as Gtk
import qualified Graphics.UI.Gtk.Gdk.Screen         as Gtk
import qualified Graphics.UI.Gtk.General.General    as Gtk
import qualified Graphics.UI.Gtk.Misc.EventBox      as Gtk
import qualified Graphics.UI.Gtk.Windows.Window     as Gtk

#if ! USE_CAIRO_SURFACE_BUFFER
import qualified Graphics.UI.Gtk.Gdk.GC             as Gtk
import qualified Graphics.UI.Gtk.Gdk.Pixmap         as Gtk
#endif

import           Diagrams.Backend.Cairo.Internal
import           Diagrams.BoundingBox
import           Diagrams.Core.Compile
import           Diagrams.Core.Types              (Diagram)
import           Diagrams.Size                    (dims)

import           Linear.Affine
import           Linear.V2 (V2(..))

import           System.IO
import           System.IO.Unsafe

import qualified System.Glib.Signals                as Glib
import qualified System.Glib.Utils                  as Glib
import qualified System.Glib.MainLoop               as Glib

import           Debug.Trace

----------------------------------------------------------------------------------------------------

-- The debugging system is built-in to this module in a way that the debugging code cannot be
-- separated from the function of the program. However you can select which debugging information to
-- include. Constant folding and dead code elimination passes in the optimizer will ensure that ONLY
-- the debug messages that are selected are included in the compiled binary program.

debugThisModule :: DebugTag
debugThisModule  = mempty

------------------

-- Debug function selection tags
_mousevt, _keyevt, _animevt, _notanimevt, _drawevt, _winevt, _allevt, _ctxevt :: DebugTag
_setup, _locks, _infra,  _fulldebug, _allbutanim :: DebugTag
_miscA, _miscB :: DebugTag

_mousevt    = DebugTag 0x00000001 -- only mouse events
_keyevt     = DebugTag 0x00000002 -- only key events
_animevt    = DebugTag 0x00000004 -- only animation events
_drawevt    = DebugTag 0x00000008 -- only redraw events
_ctxevt     = DebugTag 0x00000100 -- only context switching events
_winevt     = DebugTag 0x00000200 -- window manager related events
_allevt     = _mousevt <> _keyevt <> _animevt <> _drawevt <> _winevt <> _ctxevt
_notanimevt = _exclude _animevt _allevt -- all events apart from animation events
_locks      = DebugTag 0x00010000 -- MVar locking and unlocking  -- WARNING: extremely verbose!!!
_infra      = DebugTag 0x00020000 -- the callback infrastructure -- WARNING: extremely verbose!!!
_setup      = DebugTag 0x00040000
_miscA      = DebugTag 0x01000000 -- is used to temporarily force a debug message
_miscB      = DebugTag 0x02000000 -- is used to temporarily force a debug message
_fulldebug  = _setup <> _allevt <> _locks <> _infra <> _miscA <> _miscB
_allbutanim = _exclude _animevt _fulldebug

-- Two miscelanea tags are provided, '_miscA' and '_miscB'. When looking through the source code in
-- this file, if you find a debug logging function that you want to force to output, even if it's
-- ordinary tag is not selected, edit the source code on that log function to match one of the
-- miscelanea tags. For example, if you want to report on all calls to 'Cairo.renderWith' but not
-- report on any of the other '_drawevt' tags, then wherever there is a @(logIO _drawevt)@
-- function reporting on the call of of 'Cairo.renderWith', change the tag to
-- @(logIO (_drawevt<>_miscA))@ and then set 'debugThisModule' to '_miscA'.

newtype DebugTag = DebugTag Word32 deriving (Eq, Bits)
instance Semigroup DebugTag where { (<>) = (.|.); }
instance Monoid DebugTag where { mempty = DebugTag 0; mappend = (<>); }

-- | Exclude A from B
_exclude :: DebugTag -> DebugTag -> DebugTag
_exclude excluded selector = selector .&. complement excluded

----------------------------------------------------------------------------------------------------

type Log m = DebugTag -> String -> m ()

mkLogger
  :: (Monad m, MonadIO m)
  => String -> m (Log IO)
mkLogger func = return $ \ sel msg -> if sel .&. debugThisModule == mempty then return () else do
  tid <- myThreadId
  traceM $ '[':show tid++"][Happlets.Lib.Gtk."++func++']':msg
{-# INLINE mkLogger #-}

logSubIO :: Log IO -> DebugTag -> String -> IO a -> IO a
logSubIO logIO sel msg f = logIO sel ("[BEGIN] "++msg) >> f <* logIO sel ("[ END ] "++msg)
{-# INLINE logSubIO #-}

logSubGtk :: Log IO -> DebugTag -> String -> GtkState a -> GtkState a
logSubGtk logIO sel msg f = get >>=
  liftIO . logSubIO logIO sel msg . runGtkState f >>= state . const
{-# INLINE logSubGtk #-}

logModMVar :: Log IO -> DebugTag -> String -> MVar a -> (a -> IO (a, b)) -> IO b
logModMVar logIO sel label mvar =
  logSubIO logIO (_locks<>sel) ("modifyMVar "++label) . modifyMVar mvar
{-# INLINE logModMVar #-}

logModMVar_ :: Log IO -> DebugTag -> String -> MVar a -> (a -> IO a) -> IO ()
logModMVar_ logIO sel label mvar =
  logSubIO logIO (_locks<>sel) ("modifyMVar_ "++label) . modifyMVar_ mvar
{-# INLINE logModMVar_ #-}

logWithMVar :: Log IO -> DebugTag -> String -> MVar a -> (a -> IO b) -> IO b
logWithMVar logIO sel label mvar =
  logSubIO logIO (_locks<>sel) ("withMVar "++label) . withMVar mvar
{-# INLINE logWithMVar #-}

----------------------------------------------------------------------------------------------------

#if ! USE_CAIRO_SURFACE_BUFFER
dToW16 :: Double -> Word16
dToW16 = round . (*) (realToFrac (maxBound::Word16))

dToGrey :: Double -> Gtk.Color
dToGrey d = Gtk.Color (dToW16 d) (dToW16 d) (dToW16 d)
#endif

-- | This is the frame rate used by default when installing an animation event handler. This value
-- can be configured by modifying the 'Happlets.Config.animationFrameRate'.
gtkAnimationFrameRate :: Double
gtkAnimationFrameRate = 30.0

----------------------------------------------------------------------------------------------------

-- | A monadic wrapper around a 'Cario.Render' monad. The only reason for this to exist is because
-- there needs to be an instance of 'Data.Semigroup.Semigroup'.
--
-- The 'Controller' type is defined to use the 'CairoRender' as it's @view@ type. You can also
-- convert a 'GtkCairoDiagram' to a 'CairoRender' using the 'gtkCairoDiagram' function.
newtype CairoRender a = CairoRender (StateT CairoRenderState Cairo.Render a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance Semigroup a => Semigroup (CairoRender a) where
  (CairoRender a) <> (CairoRender b) = CairoRender $ (<>) <$> a <*> b

instance Monoid a => Monoid (CairoRender a) where
  mappend (CairoRender a) (CairoRender b) = CairoRender $ mappend <$> a <*> b
  mempty = return mempty

-- | Mose Cairo commands work in 'VectorMode' where no special state is necessary. But the
-- 'Happlets.Draw.setPoint' and 'Happlets.Draw.getPoint' commands operate in 'RasterMode', where the
-- vector operations need to be flushed before beginning, and where the updated pixels need to be
-- marked before returning to 'VectorMode'.
data CairoRenderMode
  = VectorMode
  | RasterMode !(V2 Double) !(V2 Double)

data CairoRenderState
  = CairoRenderState
    { cairoKeepWinSize           :: !PixSize
    --  , theMinFontExtents          :: !(Maybe Cairo.FontExtents)
    , theCairoRenderMode         :: !CairoRenderMode
    , theCairoScreenPrinterState :: !ScreenPrinterState
    }

--minFontExtents :: Lens' CairoRenderState (Maybe Cairo.FontExtents)
--minFontExtents = lens theMinFontExtents $ \ a b -> a{ theMinFontExtents = b }

cairoRenderMode :: Lens' CairoRenderState CairoRenderMode
cairoRenderMode = lens theCairoRenderMode $ \ a b -> a{ theCairoRenderMode = b }

cairoScreenPrinterState :: Lens' CairoRenderState ScreenPrinterState
cairoScreenPrinterState = lens theCairoScreenPrinterState $ \ a b ->
  a{ theCairoScreenPrinterState = b }

-- | Lift a @"Graphics.Rendering.Cairo".'Cairo.Render'@ function type into the 'CairoRender'
-- function type.
cairoRender :: Cairo.Render a -> CairoRender a
cairoRender = CairoRender . lift

-- | Extract a @"Graphics.Rendering.Cairo".'Cairo.Render'@ from a 'CairoRender' function type.
runCairoRender
  :: PixSize -> CairoRenderState -> CairoRender a -> Cairo.Render (a, CairoRenderState)
runCairoRender winsize rendst (CairoRender f) =
  runStateT (lift (cairoSetFontStyle defaultFontStyle) >> f) (rendst{ cairoKeepWinSize = winsize })

-- | Switch to vector mode (only if it isn't already) which marks the surface as "dirty" in the
-- smallest possible rectangle containing all points. This function is called before every vector
-- operation.
vectorMode :: CairoRender ()
vectorMode = CairoRender $ use cairoRenderMode >>= \ case
  VectorMode       -> return ()
  RasterMode lo hi -> do
    let (V2 loX loY) = round <$> lo
        (V2 hiX hiY) = round <$> hi
    lift $ Cairo.withTargetSurface $ \ surface ->
      Cairo.surfaceMarkDirtyRectangle surface loX loY (hiX - loX + 1) (hiY - loY + 1)
    cairoRenderMode .= VectorMode

-- | Switch to raster mode (only if it isn't already) which flushs all pending vector events. This
-- function is called before every raster operation.
rasterMode :: Double -> Double -> CairoRender ()
rasterMode x y = CairoRender $ use cairoRenderMode >>= \ case
  RasterMode (V2 loX loY) (V2 hiX hiY) -> do
    cairoRenderMode .= RasterMode (V2 (min loX x) (min loY y)) (V2 (max hiX x) (max hiY y))
  VectorMode                           -> do
    lift cairoFlush
    cairoRenderMode .= RasterMode (V2 x y) (V2 x y)

----------------------------------------------------------------------------------------------------

-- | This data type acts as a handle to a Gtk+ environment, which contains pointers to the window
-- and graphpics contexts which you can manipulate by inserting or removing your own Happlets to be
-- displayed within it.
data GtkWindow
  = GtkUnlockedWin !(MVar GtkWindowState)
    -- ^ This is the constructor used by public-facing APIs. The 'Happlets.Initialize.newWindow'
    -- 'Happlets.Initialize.deleteWindow', and 'Happlets.Initialize.attachWindow' functions all
    -- return or pass this constructor around.
  | GtkLockedWin   !(GtkWindowState)
    -- ^ This is the constructor use internally, particularly when evaluating the 'Happlets.GUI.GUI'
    -- monad. When evaluation completes, it is stored back into the MVar for 'GtkUnlockedWin' and the
    -- 'GtkUnlockedWin' is returned instead.

-- | Contains parameters that can exist /after/ the 'Gtk.Window' has been allocated. These
-- parameters are stored in an 'MVar' which is initialized in an empty state until the Gtk+ window
-- is actually made visible (when it recieves the first "configure" event). The reason for this is
-- that many of the 'Happlets.GUI.GUI' functions may try to evaluate a redraw before the window
-- exists and the DrawWIndow has been allocated.
data GtkWindowLive
  = GtkWindowLive
    { gtkDrawWindow   :: !Gtk.DrawWindow
#if USE_CAIRO_SURFACE_BUFFER
    , theCairoSurface :: !Cairo.Surface
      -- ^ This is the 'Cairo.Surface' that contains the buffer of the image that is displayed in
      -- the Happlet window. Every time the window is resized, this object is re-allocated, so it is
      -- important that this value be accessible in only one location, do not store a copy of it
      -- into an MVar or IORef which other threads my write to arbitrarily, unless you like the
      -- image to be garbled.
#else
    , theGtkPixmap    :: !Gtk.Pixmap
      -- ^ This is the 'Gtk.Pixmap' that contains the buffer of the image that is displayed in the
      -- Happlet window. Every time the window is resized, this object is re-allocated, so it is
      -- important that this value be accessible in only one location, do not store a copy of it
      -- into an MVar or IORef which other threads may write to arbitrarily, unless you like the
      -- image to be garbled.
    , theGtkGraphCtx  :: !Gtk.GC
      -- ^ This is the old-fashioned GUI graphics context, which keeps track of things like
      -- foreground color, background color, clip region, and pen location. This is used for Gdk
      -- (not Gtk+) drawing primitives. As far as I know, this data structure is deprecated and does
      -- not even exist in Gtk+ version 3, but it is still the only way to perform certain
      -- graphics operations in Gtk+ version 2.
#endif
    }

data GtkWindowState
  = GtkWindowState
    { currentConfig          :: !Config
    , thisWindow             :: !(MVar GtkWindowState)
    , gtkWindowLive          :: !(MVar GtkWindowLive)
    , gtkWindow              :: !Gtk.Window
    , theGtkEventBox         :: !Gtk.EventBox
    , theCairoRenderState    :: !CairoRenderState
    , theInitReaction        :: !(ConnectReact PixSize)
    , theResizeReaction      :: !(ConnectReact PixCoord)
    , theVisibilityReaction  :: !(ConnectReact Bool)
    , theFocusReaction       :: !(ConnectReact Bool)
    , theMouseHandler        :: !(ConnectReact Mouse)
    , theKeyHandler          :: !(ConnectReact Keyboard)
    , theAnimatorThread      :: !(ConnectReact AnimationMoment)
    , theContextSwitcher     :: GUIContinue -> GtkWindowState -> IO (GUIContinue, GtkWindowState)
    }

data ConnectReact event
  = Disconnected
  | ConnectReact{ doDisconnect :: IO (), doReact :: event -> GtkState () }

evalConnectReact
  :: Log IO
  -> DebugTag
  -> String
  -> Lens' GtkWindowState (ConnectReact event)
  -> event
  -> GtkState ()
evalConnectReact logIO sel what connectReact event = use connectReact >>= \ case
  Disconnected -> liftIO $
    logIO (_infra<>sel) $ "evalConnectReact "++what++" -- is not connected"
  ConnectReact{doReact=f} ->
    logSubGtk logIO (_infra<>sel) ("evalConnectReact "++what) $ f event

forceDisconnect
  :: Log IO
  -> DebugTag
  -> String
  -> Lens' GtkWindowState (ConnectReact event)
  -> GtkState ()
forceDisconnect logIO sel what connectReact = use connectReact >>= \ case
  ConnectReact{doDisconnect=discon} -> do
    liftIO $ logSubIO logIO (_infra<>sel) ("forceDisconnect "++what) discon
    connectReact .= Disconnected
  Disconnected -> liftIO $
    logIO (_infra<>sel) $ "forceDisconnect "++what++" -- already disconnected"

gtkEventBox :: Lens' GtkWindowState Gtk.EventBox
gtkEventBox = lens theGtkEventBox $ \ a b -> a{ theGtkEventBox = b }

cairoRenderState :: Lens' GtkWindowState CairoRenderState
cairoRenderState = lens theCairoRenderState $ \ a b -> a{ theCairoRenderState = b }

initReaction :: Lens' GtkWindowState (ConnectReact PixSize)
initReaction = lens theInitReaction $ \ a b -> a{ theInitReaction = b }

visibilityReaction :: Lens' GtkWindowState (ConnectReact Bool)
visibilityReaction = lens theVisibilityReaction $ \ a b -> a{ theVisibilityReaction = b }

focusReaction :: Lens' GtkWindowState (ConnectReact Bool)
focusReaction = lens theFocusReaction $ \ a b -> a{ theFocusReaction = b }

resizeReaction :: Lens' GtkWindowState (ConnectReact PixCoord)
resizeReaction = lens theResizeReaction $ \ a b -> a{ theResizeReaction = b }

mouseHandler :: Lens' GtkWindowState (ConnectReact Mouse)
mouseHandler = lens theMouseHandler $ \ a b -> a{ theMouseHandler = b }

keyHandler :: Lens' GtkWindowState (ConnectReact Keyboard)
keyHandler = lens theKeyHandler $ \ a b -> a{ theKeyHandler = b }

animatorThread :: Lens' GtkWindowState (ConnectReact AnimationMoment)
animatorThread = lens theAnimatorThread $ \ a b -> a{ theAnimatorThread = b }

contextSwitcher
  :: Lens' GtkWindowState
      (GUIContinue -> GtkWindowState -> IO (GUIContinue, GtkWindowState))
contextSwitcher = lens theContextSwitcher $ \ a b -> a{ theContextSwitcher = b }

-- Create an image buffer with an alpha channel for a 'Graphics.UI.Gtk.Abstract.Widget.Widget' to
-- be rendered.
mkAlphaChannel :: Log IO -> Gtk.Widget -> IO ()
mkAlphaChannel logIO window = do
  logIO _setup $ "Gtk.widgetGetScreen"
  screen <- Gtk.widgetGetScreen window
  logIO _setup $ "Gtk.screenGetRGBAColormap"
  colormap <- Gtk.screenGetRGBAColormap screen
  case colormap of
    Nothing       -> do
      logIO _setup $ "-- Colormap dos NOT support alpha channels"
      return ()
    Just colormap -> do
      logIO _setup $ "Gtk.widgetSetColorMap"
      Gtk.widgetSetColormap window colormap

----------------------------------------------------------------------------------------------------

newtype GtkState a
  = GtkState { unwrapGtkState :: StateT GtkWindowState IO a }
  deriving (Functor)

instance Applicative GtkState where
  pure = return
  (<*>) = ap

instance Monad GtkState where
  return = GtkState . return
  (GtkState a) >>= b = GtkState $ a >>= unwrapGtkState . b

instance MonadState GtkWindowState GtkState where
  state = GtkState . state

instance MonadIO GtkState where
  liftIO = GtkState . liftIO

runGtkState :: GtkState a -> GtkWindowState -> IO (a, GtkWindowState)
runGtkState (GtkState f) = runStateT f

-- | This function acquires a lock on the 'GtkState' and begins evaluating the 'GtkState'
-- function. This function should be evaluated at the top level of the procedure that is evaluated
-- by an event handler.
lockGtkWindow :: Log IO -> DebugTag -> GtkWindow -> GtkState a -> IO a
lockGtkWindow logIO sel win f = case win of
  GtkLockedWin{} -> error "lockGtkWindow: evaluated on an already locked 'GtkLockedWin' window."
  GtkUnlockedWin mvar -> logModMVar logIO (_locks<>sel) "GtkUnlockedWin" mvar $
    liftM (\ (a, b) -> (b, a)) . runGtkState f

-- | This function evaluates 'Happlets.GUI.GUI' functions within event handlers. It evaluates to a
-- 'GtkState' function, which means you are required to have first evaluated 'unlockGtkWindow'. This
-- function will then lock the 'Happlets.GUI.Happlet' and then evaluate the 'Happlets.GUI.GUI'
-- function. If the 'Happlets.GUI.GUI' function evaluates to 'Happlets.GUI.disable' or
-- 'Happlets.GUI.failGUI', then this function returns 'Prelude.False'. Otherwise, 'Prelude.True' is
-- returned.
gtkRunHapplet :: Happlet model -> GtkGUI model void -> GtkState GUIContinue
gtkRunHapplet happ f = do
  logIO <- mkLogger "gtkRunHapplet"
  -- Set a default context switcher.
  contextSwitcher .= curry return
  env <- get
  liftIO $ logIO _locks $ "locking Happlet, evaluating GUI function..."
  ((env, guiContin), _) <- liftIO $ onHapplet happ $ \ model -> do
    gui <- logSubIO logIO _infra "evalGUI -- begin callback" $
      evalGUI f happ (GtkLockedWin env) model
    let env = case theGUIWindow gui of
          GtkLockedWin env -> env
          GtkUnlockedWin{} -> error
            "gtkRunHapplet: evalGUI evaluated on GtkLockedWin but returned GtkUnlockedWin"
    return ((env, theGUIContinue gui), theGUIModel gui)
  (guiContin, env) <- liftIO $ theContextSwitcher env guiContin env
  state $ const (guiContin, env & contextSwitcher .~ curry return)

-- | This function is intended to be passed as a parameter to 'gtkRunHapplet' by event handling
-- functions which check if the 'Happlets.GUI.GUI' function evaluated to 'Happlets.GUI.disable'.
checkGUIContinue
  :: Log IO
  -> DebugTag
  -> String
  -> Lens' GtkWindowState (ConnectReact event)
  -> GUIContinue
  -> GtkState ()
checkGUIContinue logIO sel what connectReact = \ case
  GUIHalt     ->
    forceDisconnect logIO (_infra<>sel) (what++"(on GUIHalt)") connectReact
  GUIFail msg ->
    forceDisconnect logIO (_infra<>sel) (what++"(on GUIFail "++show msg++")") connectReact
  GUICancel   -> return ()
  GUIContinue -> liftIO $ logIO (_infra<>sel) $ what++"-- evaluated to 'GUIContinue'"

-- | This function should be evaluated from within a 'GtkGUI' function when it is necessary to
-- update the 'GtkWindowState', usually this is for installing or removing event handlers.
runGtkStateGUI :: DebugTag -> String -> GtkState a -> GtkGUI model a
runGtkStateGUI sel what f = do
  logIO <- mkLogger "runGtkStateGUI"
  getGUIState >>= \ gui -> case theGUIWindow gui of
    GtkUnlockedWin{} -> error $ "runGtkStateGUI: " ++
      "Evaluated a GtkState function within a GtkGUI function on a locked GtkWindow."
      -- The 'window' value passed to the 'evalGUI' function must be a 'GtkLockedWin' constructor.
    GtkLockedWin env -> do
      (a, env) <- liftIO $ logSubIO logIO (_infra<>sel) ("runGtkState "++what) $
        runGtkState f env
      putGUIState $ gui{ theGUIWindow = GtkLockedWin env }
      return a

-- | Create the window and install the permanent event handlers.
createWin :: Config -> IO GtkWindow
createWin cfg = do
  logIO <- mkLogger "createWin"
  forM_ (configErrorsOnLoad cfg) (hPrint stderr)
  -- new window --
  logIO _setup $ "Gtk.windowNew"
  window <- Gtk.windowNew
  logIO _setup $ "Gtk.widgetSetHasWindow"
  Gtk.widgetSetHasWindow window True
  logIO _setup $ "Gtk.widgetSetAppPaintable"
  Gtk.widgetSetAppPaintable window True
  logIO _setup $ "Gtk.widgetSetDoubleBuffered window False"
  Gtk.widgetSetDoubleBuffered window False
  when (isJust $ cfg ^. backgroundTransparency) $
    mkAlphaChannel logIO $ Gtk.castToWidget window
  logIO _setup $ "Gtk.windowSetTypeHint GtkWindowTtypeHintNormal"
  Gtk.windowSetTypeHint window Gtk.WindowTypeHintNormal
  logIO _setup $ "Gtk.windowSetDefaultSize " ++ show (cfg ^. recommendWindowSize)
  uncurry (Gtk.windowSetDefaultSize window) $ cfg ^. recommendWindowSize
  logIO _setup $ "Gtk.windowSetDecorated " ++ show (cfg ^. decorateWindow)
  Gtk.windowSetDecorated window $ cfg ^. decorateWindow
  eventBox <- Gtk.eventBoxNew
  Gtk.eventBoxSetVisibleWindow eventBox False
  Gtk.eventBoxSetAboveChild    eventBox True
  logIO _setup $ "Gtk.containerAdd window eventBox"
  Gtk.containerAdd window eventBox
  this <- newEmptyMVar
  live <- newEmptyMVar
  let env = GtkWindowState
        { currentConfig            = cfg
        , thisWindow               = this
        , gtkWindowLive            = live
        , gtkWindow                = window
        , theGtkEventBox           = eventBox
        , theCairoRenderState      = CairoRenderState
            { cairoKeepWinSize           = V2 0 0
            , theCairoRenderMode         = VectorMode
            , theCairoScreenPrinterState = screenPrinterState
            }
        , theInitReaction          = Disconnected
        , theResizeReaction        = Disconnected
        , theVisibilityReaction    = Disconnected
        , theFocusReaction         = Disconnected
        , theMouseHandler          = Disconnected
        , theKeyHandler            = Disconnected
        , theAnimatorThread        = Disconnected
        , theContextSwitcher       = curry return
        }
  ((), env) <- flip runGtkState env $ do
    installExposeEventHandler
    installDeleteEventHandler
    installInitEventHandler
  putMVar this env
  return $ GtkUnlockedWin $ thisWindow env

-- | Expose event handlers do not evaluate any 'GUI' functions. The expose event handler's only task
-- is to blit the canvas pixmap to the window's image buffer.
--
-- The expose event handler installed here actually will remove itself and install another expose
-- event handler which will be used throughout the remainder of the windows lifetime. The reason
-- this is necessary due to some idiosyncratic behavior of Gtk+, namely that the first time the
-- expose event handler is called, the window's image buffer has not been properly allocated and the
-- event handler that does the blitting will be needing a pointer to an image buffer, so the
-- permanent event handler cannot be installed until the pointer to the image buffer becomes
-- available. 
installExposeEventHandler :: GtkState ()
installExposeEventHandler = do
  logIO <- mkLogger "installExposeEventHandler"
  env <- get
  liftIO $ do
    --logGUI $ "Gtk.widgetAddEvents env [Gtk.StructureMask]"
    --Gtk.widgetAddEvents (gtkWindow env) [Gtk.StructureMask, Gtk.ExposureMask]
    --------------------------------------- Expose Event -----------------------------------
    logIO _setup "Glib.on Gtk.exposeEvent -- for init handler"
    initExposeEventMVar <- newEmptyMVar
    (>>= (putMVar initExposeEventMVar)) $ Glib.on (gtkWindow env) Gtk.exposeEvent $ do
      logIO <- mkLogger "initExposeEventCallback"
      liftIO $ do
        logIO _drawevt "Gtk.eventWindow >>= putMVar gtkDrawWindowMVar"
        takeMVar initExposeEventMVar >>= Glib.signalDisconnect
        logIO _drawevt "Glib.on window Gtk.exposeEvent"
        Glib.on (gtkWindow env) Gtk.exposeEvent $ do
          logIO  <- mkLogger "exposeEventHandler"
          canvas <- Gtk.eventWindow
          region <- Gtk.eventRegion
          liftIO $ logWithMVar logIO _drawevt "gtkWindowLive" (gtkWindowLive env) $
            exposeEventHandler logIO canvas region
          return True
        return True

-- | The init event handler was responsible for pulling the init GUI function from the
-- 'GtkWindowState' and evaluating it, then installing a configure event handler. 
installInitEventHandler :: GtkState ()
installInitEventHandler = do
  logIO <- mkLogger "permanentHandlers"
  env <- get
  liftIO $ do
    ------------------------------------ Initializing Event --------------------------------
    logIO _setup "Glib.on Gtk.configureEvent -- for init handler"
    initHandler <- newEmptyMVar
    (>>= (putMVar initHandler)) $ Glib.on (gtkWindow env) Gtk.configureEvent $ do
      logIO <- mkLogger "initCallback"
      canvas <- Gtk.eventWindow
      size   <- Gtk.eventSize
      let allocSize = dimsForAlloc size
      let evt = sampCoord <$> uncurry V2 size
      liftIO $ lockGtkWindow logIO _setup (GtkUnlockedWin $ thisWindow env) $ do
        live   <- gets gtkWindowLive
        liftIO $ do
          logIO _setup $
            "newGtkWindowLive (dimsForAlloc " ++ show size ++ " --> " ++ show allocSize ++ ")"
          livest <- newGtkWindowLive (currentConfig env) canvas allocSize
          logIO _setup "putMVar gtkWindowLive"
          putMVar live livest
          logIO _setup $ "initReaction " ++ show size
        evalConnectReact logIO _setup "initReaction" initReaction evt
        liftIO $ do
          logIO _setup "Glib.signalDisconnect initCallback"
          takeMVar initHandler >>= Glib.signalDisconnect
          logIO _setup "installResizeEventHandler -- a permanent handler"
        installResizeEventHandler
      return False

-- | This function installs the configure event handler, which is called whenever the window is
-- resized.
installResizeEventHandler :: GtkState ()
installResizeEventHandler = do
  logIO <- mkLogger "installResizeEventHandler"
  env <- get
  liftIO $ do
    logIO _setup "Glib.on window Gtk.configureEvent"
    void $ Glib.on (gtkWindow env) Gtk.configureEvent $ do
      logIO  <- mkLogger "configureEventCallback"
      canvas <- Gtk.eventWindow
      size   <- Gtk.eventSize
      let evt = sampCoord <$> uncurry V2 size
      liftIO $ lockGtkWindow logIO _winevt (GtkUnlockedWin $ thisWindow env) $ do
        logSubGtk logIO _winevt ("resizeGtkDrawContext "++show size) $
          resizeGtkDrawContext canvas size
        evalConnectReact logIO _winevt "resizeReaction" resizeReaction evt
        liftIO $ do
          logIO _drawevt "Gtk.widgetQueueDraw"
          Gtk.widgetQueueDraw (gtkWindow env)
          return False

-- | This event handler is responsible for cleaning up when a window close event occurs. Whether the
-- application quit or not when the window closes is configurable by parameters in the
-- 'Happlets.Config.Config' data structure.
installDeleteEventHandler :: GtkState (Glib.ConnectId Gtk.Window)
installDeleteEventHandler = do
  logIO <- mkLogger "installDeleteEventHandler"
  env <- get
  liftIO $ do
    logIO _setup "Glib.on Gtk.deleteEvent"
    Glib.on (gtkWindow env) Gtk.deleteEvent $ liftIO $
      lockGtkWindow logIO _winevt (GtkUnlockedWin $ thisWindow env) $ do
        env <- get
        liftIO $ do
          logIO <- mkLogger "deleteWindowCallback"
          let cfg = currentConfig env
          if cfg ^. deleteWindowOnClose
           then do
            logIO _winevt "Gtk.widgetDestroy -- willDeleteWindowOnClose is True"
            Gtk.widgetDestroy (gtkWindow env)
           else liftIO $ do
            logIO _winevt $ "Gtk.widgetHideAll -- willDeleteWindowOnClose is False"
            Gtk.widgetHideAll (gtkWindow env)
          if not (cfg ^. quitOnWindowClose)
           then logIO _winevt "-- quitOnWindowClose is False"
           else do
            logIO _winevt $ "Gtk.mainQuit -- quitOnWindowClose is True"
            Gtk.mainQuit
          return True

deleteWin :: GtkWindowState -> IO ()
deleteWin env = do
  logIO <- mkLogger "deleteWin"
  logIO _winevt "Gtk.widgetDestroy"
  Gtk.widgetDestroy (gtkWindow env)

--evalGtkDraw :: GtkDraw -> GtkState ()
--evalGtkDraw (CairoRender draw) = get >>= \ env -> liftIO $ case theGtkPixmap env of
--  Just pixmap -> Gtk.renderWithDrawable pixmap draw
--  Nothing     -> Gtk.widgetGetDrawWindow (gtkWindow env) >>= flip Gtk.renderWithDrawable draw

-- | Compute rectangular dimensions for a window buffer allocation that are large enough to contain
-- the given rectangluar dimensions, but are also an even multiple of some unit value. When resizing
-- windows with 'resizeGtkDrawContext' this function is used to decide whether it is necessary to
-- re-allocate the window buffer or if the old window buffer is still large enough to contain the
-- new window dimensions.
dimsForAlloc :: (Int, Int) -> (Int, Int)
dimsForAlloc (w, h) = (step w, step h) where
  factor = 32
  step a = (div (abs a) factor + 1) * factor
  -- For 32-bit pixels, with a minimum alloctable rectangle size of 32*32 means the minimum buffer
  -- allocation will be 32*32*32 = 32^3 = (2^5)^3 = 2^15 = 32768 bytes.

-- | This function obtains a widget drawing window object and it's size, and allocates a new pixmap
-- buffer and GDK drawing context for it. This function conditionally calls 'newGtkWindowLive' if
-- 'dimsForAlloc' returns a different value than what is already allocated. This function also locks
-- the 'gtkWindowLive' MVar, so it is assumed the MVar is not empty. So this function must not be
-- called until the "initCallback" has been evaluated.
resizeGtkDrawContext :: Gtk.DrawWindow -> (Int, Int) -> GtkState ()
resizeGtkDrawContext canvas size = do
  logIO <- mkLogger "resizeGtkDrawContext"
  env <- get
  let cfg = currentConfig env
  let newDims = dimsForAlloc size
  liftIO $ logModMVar_ logIO _drawevt "gtkWindowLive" (gtkWindowLive env) $ \ livest -> do
#if USE_CAIRO_SURFACE_BUFFER
    let surface = theCairoSurface livest
    oldDims <- (,)
      <$> Cairo.imageSurfaceGetWidth  surface
      <*> Cairo.imageSurfaceGetHeight surface
#else
    oldDims <- Gtk.drawableGetSize (theGtkPixmap livest)
#endif
    if newDims == oldDims
     then do
      logIO _drawevt $ "-- will not re-allocate window buffer, old buffer size = "++
        show oldDims++", new buffer size = "++show newDims
      return livest
     else do
      logIO _drawevt $ "newGtkWindowLive "++show newDims
      newGtkWindowLive cfg canvas newDims

-- | Allocates a new 'GtkWindowLive', including the 'Gtk.Pixmap' buffer and 'Gtk.GC' graphics
-- context. The buffer allocated will be exactly the dimensions given without checking if the size
-- is valid, and invalid demensions will crash the thread.
newGtkWindowLive :: Config -> Gtk.DrawWindow -> (Int, Int) -> IO GtkWindowLive
newGtkWindowLive cfg canvas (w, h) = do
  logIO <- mkLogger "gtkAllocNewPixmap"
#if USE_CAIRO_SURFACE_BUFFER
  let depth = if isJust $ cfg ^. backgroundTransparency
        then Cairo.FormatARGB32
        else Cairo.FormatRGB24
  logIO _winevt $ unwords ["Cairo.createImageSurface", show depth, show w, show h]
  surface <- Cairo.createImageSurface depth w h
#else
  let grey  = cfg ^. backgroundGreyValue
  let depth = Just $ if isJust $ cfg ^. backgroundTransparency then 32 else 24
  logIO _winevt $ "Gtk.pixmapNew " ++ show (w, h) ++ ' ' : show depth
  pixmap <- Gtk.pixmapNew (Just canvas) w h depth
  gc <- liftIO $ Gtk.gcNewWithValues canvas Gtk.newGCValues
    { Gtk.background = dToGrey grey
    , Gtk.tile       = Just pixmap
    , Gtk.fill       = Gtk.Tiled
    }
#endif
  return GtkWindowLive
    { gtkDrawWindow   = canvas
#if USE_CAIRO_SURFACE_BUFFER
    , theCairoSurface = surface
#else
    , theGtkPixmap    = pixmap
    , theGtkGraphCtx  = gc
#endif
    }

disconnectAll :: Log IO -> GtkState ()
disconnectAll logIO = logSubGtk logIO _allevt "disconnectAll" $ do
  forceDisconnect logIO _winevt  "visibilityReaction" visibilityReaction
  forceDisconnect logIO _winevt  "focusReaction"      focusReaction
  forceDisconnect logIO _mousevt "mouseHandler"       mouseHandler
  forceDisconnect logIO _keyevt  "keyHandler"         keyHandler
  forceDisconnect logIO _animevt "animatorThread"     animatorThread
  forceDisconnect logIO _winevt  "resizeReaction"     resizeReaction

----------------------------------------------------------------------------------------------------

-- | This is a type of 'Happlets.GUI.GUI' function where the @window@ type is 'GtkWindow' and the
-- @draw@ type is 'GtkDraw' (a Cairo 'Graphics.Rendering.Cairo.Render' function).
type GtkGUI model a = GUI GtkWindow model a

-- | Creates a happlet and associates it with a window.
gtkNewWindow :: Config -> IO GtkWindow
gtkNewWindow cfg = do
  logIO <- mkLogger "gtkWindowNew"
  logIO _winevt "createWin"
  createWin cfg

gtkWindowVisible :: Bool -> GtkGUI model ()
gtkWindowVisible visible = do
  logIO <- mkLogger ("gtkWindowVisible "++show visible)
  runGtkStateGUI _winevt ("gtkWindowVisible "++show visible) $ do
    env <- get
    liftIO $ if visible
     then do
      logIO _winevt "Gtk.widgetShow"
      Gtk.widgetShowAll (gtkWindow env)
      logIO _winevt "Gtk.widgetQueueDraw"
      Gtk.widgetQueueDraw (gtkWindow env)
     else do
      logIO _winevt "Gtk.widgetHideAll"
      Gtk.widgetHideAll (gtkWindow env)

gtkAttachHapplet :: Bool -> GtkWindow -> Happlet model -> (PixSize -> GtkGUI model ()) -> IO ()
gtkAttachHapplet showWin win happ init = do
  logIO <- mkLogger "gtkAttachHapplet"
  lockGtkWindow logIO _ctxevt win $ do
    disconnectAll logIO
    initReaction .= ConnectReact
      { doDisconnect = return () -- This function is disconnected automatically by the init callback.
      , doReact = \ size -> do
          logIO <- mkLogger "initReaction"
          env <- get
          guiContin <- logSubGtk logIO _ctxevt ("gtkRunHapplet "++show size) $
            gtkRunHapplet happ $ init size
          when (showWin && guiContin == GUIContinue) $ liftIO $ do
            logIO _drawevt "Gtk.widgetShowAll"
            Gtk.widgetShowAll (gtkWindow env)
            logIO _drawevt "Gtk.widgetQueueDraw"
            Gtk.widgetQueueDraw (gtkWindow env)
          case guiContin of
            GUIFail msg -> do
              -- TODO: show a modal dialog box reporting the error.
              liftIO $ hPutStrLn stderr msg
            _           -> return ()
      }
    win <- gets gtkWindow
    liftIO $ do
      logIO _winevt "Gtk.widgetShow"
      Gtk.widgetShow win

-- | Change the happlet displayed in the current window. Remove the current Happlet event handlers
-- and re-install the event handlers for the given Happlet.
gtkSetHapplet :: Happlet newmodel -> (PixSize -> GtkGUI newmodel ()) -> GtkGUI oldmodel ()
gtkSetHapplet newHapp init = do
  logIO <- mkLogger "windowSetHapplet"
  oldHapp <- askHapplet
  if sameHapplet oldHapp newHapp
   then liftIO $ logIO _ctxevt $
          "-- the Happlet given is the one that is already attached to this window"
   else do
    liftIO $ logIO _ctxevt $ "putGUIState -- set context switcher"
    modifyGUIState $ guiWindow %~ \ case
      GtkUnlockedWin{} -> error "gtkSetHapplet evaluated on unlocked window"
      GtkLockedWin env -> GtkLockedWin $ env & contextSwitcher .~ \ _guiCont env ->
        logSubIO logIO _ctxevt "contextSwitcher" $ do
          size <- Gtk.widgetGetDrawWindow (gtkWindow env) >>= Gtk.drawableGetSize
          win  <- liftM fst $ onHapplet newHapp $ liftM (theGUIWindow &&& theGUIModel) .
            logSubIO logIO _ctxevt "-- call happlet initializer" .
            evalGUI (init $ sampCoord <$> uncurry V2 size) newHapp (GtkLockedWin env)
          return $ case win of
            GtkLockedWin env -> (GUIContinue, env)
              -- When 'gtkSetHapplet' is called, it must be called from within a GUI event handler.
              -- The GUI event handler is wrapped in a 'ConnectReact' data structure which is
              -- programmed to automatically call 'forceDisconnect' on itself if 'GUIHalt' is
              -- returned after evaluation of the GUI function completes within the 'runGtkState'
              -- function. However this 'contextSwitcher' function is evaluated immediately
              -- after. By default the 'contextSwitcher' will simply return the parameters passed to
              -- it. But by returning 'GUIContinue' here, 'runGtkState' will return 'GUIContinue' to
              -- the 'ConnectReact' calling context, and 'forceDisconnect' function is not
              -- called. If this function were to return 'GUIHalt', it would trigger
              -- 'forceDisconnect' in the calling context two levels above this one, which would
              -- disconnect the event handlers that have just been installed by the evaluation of
              -- the 'init' function above.
            GtkUnlockedWin{} -> error
              "'evalGUI' was given an unlocked window, but it returned a locked window."
    runGtkStateGUI _ctxevt "gtkSetHapplet" $ disconnectAll logIO
    liftIO $ logIO _ctxevt "disable"
    disable

-- Set which function redraws the window.
evalCairoOnCanvas :: CairoRender a -> GtkState a
evalCairoOnCanvas redraw = do
  logIO <- mkLogger "evalCairoOnCanvas"
  env <- get
  (a, rendst) <- liftIO $
    logWithMVar logIO _drawevt "gtkWindowLive" (gtkWindowLive env) $ \ livest -> do
      (w, h) <- Gtk.drawableGetSize (gtkDrawWindow livest)
#if USE_CAIRO_SURFACE_BUFFER
      let cairoRenderer = logSubIO logIO _drawevt "Cairo.renderWith onCanvas" .
            Cairo.renderWith (theCairoSurface livest)
#else
      let cairoRenderer = logSubIO logIO _drawevt "Gtk.renderWithDrawable onCanvas" .
            Gtk.renderWithDrawable (theGtkPixmap livest)
#endif
      (a, rendst) <- cairoRenderer $
        runCairoRender (sampCoord <$> V2 w h) (env ^. cairoRenderState) redraw
      region <- Gtk.regionRectangle $ Gtk.Rectangle 0 0 w h -- TODO: minimize the size of this region
      exposeEventHandler logIO (gtkDrawWindow livest) region livest
      --logIO _drawevt "Gtk.drawWindowInvalidateRegion"
      --Gtk.drawWindowInvalidateRegion (gtkDrawWindow livest) region True
      return (a, rendst)
  cairoRenderState .= rendst
  return a

-- Draw directly to the window.
evalCairoOnGtkDrawable :: CairoRender a -> GtkState a
evalCairoOnGtkDrawable redraw = do
  logIO <- mkLogger "evalCairoOnGtkDrawable"
  env <- get
  (a, rendst) <- liftIO $
    logWithMVar logIO _drawevt "gtkWindowLive" (gtkWindowLive env) $ \ livest -> do
      (w, h) <- Gtk.drawableGetSize (gtkDrawWindow livest)
      (a, rendst) <- logSubIO logIO _drawevt "Gtk.renderWithDrawable onOSBuffer" $
        Gtk.renderWithDrawable (gtkDrawWindow livest) $
        runCairoRender (sampCoord <$> V2 w h) (env ^. cairoRenderState) redraw
      return (a, rendst)
  cairoRenderState .= rendst
  return a

exposeEventHandler :: Log IO -> Gtk.DrawWindow -> Gtk.Region -> GtkWindowLive -> IO ()
exposeEventHandler logIO canvas region livest = do
#if USE_CAIRO_SURFACE_BUFFER
  logSubIO logIO _drawevt "Gtk.renderWithDrawable -- Gtk.exposeEvent" $
    Gtk.renderWithDrawable canvas $ do
      let surf = theCairoSurface livest
      liftIO $ logIO _drawevt $ "Cairo.setSourceSurface buffer"
      Cairo.setSourceSurface surf  0.0  0.0
      liftIO (Gtk.regionGetRectangles region) >>= mapM_
        (\ (Gtk.Rectangle x y w h) -> do
          let f = realToFrac :: Int -> Double
          liftIO $ logIO _drawevt $ unwords
            ["Cairo.rectangle", show x, show y, show w, show h, ">> Cairo.paint"]
          Cairo.setOperator Cairo.OperatorSource
          Cairo.rectangle (f x) (f y) (f w) (f h)
          Cairo.paint
        )
#else
  logIO _drawevt $ "Gtk.gcSetClipRegion"
  Gtk.gcSetClipRegion (theGtkGraphCtx livest) region
  (w, h) <- Gtk.drawableGetSize (theGtkPixmap livest)
  logIO _drawevt $ unwords ["Gtk.drawRectangle 0 0", show w, show h]
  Gtk.drawRectangle canvas (theGtkGraphCtx livest) True 0 0 w h
#endif

----------------------------------------------------------------------------------------------------

instance HappletWindow GtkWindow CairoRender where
  getWindowSize       = runGtkStateGUI _drawevt "getWindowSize" $ do
    logIO <- mkLogger "getWindowSize"
    env <- get
    liftIO $ logWithMVar logIO _drawevt "gtkWindowLive" (gtkWindowLive env) $ \ livest -> do
      -- Obtain the size from the draw window, but will not draw to the draw window.
      (w, h) <- Gtk.drawableGetSize (gtkDrawWindow livest)
      return $ V2 (sampCoord w) (sampCoord h)

  windowChangeHapplet = gtkSetHapplet
  onCanvas            = runGtkStateGUI _drawevt "onCanvas"  . evalCairoOnCanvas
  onOSBuffer          = runGtkStateGUI _drawevt "onOSBuffer". evalCairoOnGtkDrawable

  refreshRegion rects = runGtkStateGUI _drawevt "refreshRegion" $ do
    logIO <- mkLogger "refreshRegion"
    env <- get
    liftIO $ logWithMVar logIO _drawevt "gtkWindowLive" (gtkWindowLive env) $ \ livest ->
      logSubIO logIO _drawevt "Gtk.renderWithDrawable refreshRegion" $
        Gtk.renderWithDrawable (gtkDrawWindow livest) $
          forM_ (fmap realToFrac . canonicalRect2D <$> rects) $ \ rect -> do
            Cairo.setOperator Cairo.OperatorSource
            Cairo.setSourceSurface (theCairoSurface livest) (0.0) (0.0)
            let (x, y) = (rect) ^. rect2DHead . pointXY
            let (w, h) = ((rect ^. rect2DTail) - (rect ^. rect2DHead)) ^. pointXY
            Cairo.rectangle x y w h
            Cairo.fill

  refreshWindow      = runGtkStateGUI _drawevt "refreshWindow" $ do
    logIO <- mkLogger "refreshWindow"
    env <- get
    liftIO $ logWithMVar logIO _drawevt "gtkWindowLive" (gtkWindowLive env) $ \ livest -> do
      logSubIO logIO _drawevt "Gtk.renderWithDrawable refreshWindow" $
        Gtk.renderWithDrawable (gtkDrawWindow livest) $ do
          Cairo.setOperator Cairo.OperatorSource
          Cairo.setSourceSurface (theCairoSurface livest) (0.0) (0.0)
          Cairo.paint

instance Happlet2DGraphics CairoRender where
  clearScreen = unpackRGBA32Color >>> \ (r,g,b,a) -> cairoRender $ cairoClearCanvas r g b a
  drawLine a b c   = vectorMode >> cairoRender (cairoDrawLine a b c)
  drawPath a b c   = vectorMode >> cairoRender (cairoDrawPath a b c)
  drawRect a b c d = vectorMode >> cairoRender (cairoDrawRect a b c d)
  getPoint a       = cairoRender (cairoFlush >> cairoGetPoint a)
  setPoint a@(V2 (RealApprox x) (RealApprox y)) b =
    rasterMode x y >> cairoRender (cairoSetPoint a b)

instance RenderText CairoRender where
  getGridCellSize = cairoRender $ (/ 24.0) <$> cairoGetDPI
--    CairoRender $ use minFontExtents >>= \ case
--      Just ext -> return $ Cairo.fontExtentsMaxXadvance ext
--      Nothing  -> do
--        ext <- lift cairoGetMinFontExtents
--        minFontExtents .= Just ext
--        return $ (\x -> trace ("SET GRID SIZE = "++show x) x) $ Cairo.fontExtentsHeight ext / 2
    
  getWindowGridCellSize = do
    size <- getGridCellSize
    (V2 w h) <- CairoRender $ gets cairoKeepWinSize
    return $ V2 (realToFrac w / size) (realToFrac h / size)
    
  screenPrintCharNoAdvance st c = unless (not (isPrint c) || wcwidth c <= 0) $ do
    let fs = st ^. printerFontStyle
    let bgcolor = fs ^. fontBackColor
    let off@(V2 offX offY) = st ^. renderOffset
    let loc = st ^. textCursor
    loc0@(V2 x0 _y0) <- (+ (off - V2 (2.0) (3.0))) <$> gridTextLocationToPoint loc
    dLoc <- fmap realToFrac <$> getPixSizeOfChar (fs ^. fontSize) c
    rendst <- CairoRender get
    --rendst <- case rendst ^. minFontExtents of
    --  Just{}  -> return rendst
    --  Nothing -> do
    --    extns <- cairoRender $ cairoGetMinFontExtents <* cairoSetFontStyle fs
    --    CairoRender $ do
    --      minFontExtents .= Just extns
    --      cairoScreenPrinterState . printerFontStyle .= fs
    --      get
    let loc1@(V2 _x1 y1) = loc0 + dLoc
    extns  <- cairoRender $ do
      Cairo.setOperator Cairo.OperatorSource
      cairoDrawRect bgcolor (0.0) bgcolor
        (rect2D & rect2DHead .~ (realToFrac <$> loc0) & rect2DTail .~ (realToFrac <$> loc1))
      cairoSetColor $ fs ^. fontForeColor
      when (rendst ^. cairoScreenPrinterState . printerFontStyle /= fs) (cairoSetFontStyle fs)
      Cairo.fontExtents
    cairoRender $ do
      Cairo.moveTo (x0 + 0.5 + offX) (y1 - 0.5 + offY - Cairo.fontExtentsDescent extns)
      Cairo.showText (c:"")
      Cairo.fill

  saveScreenPrinterState = CairoRender . modifying cairoScreenPrinterState . const
  recallSavedScreenPrinterState = CairoRender $ use cairoScreenPrinterState

-- | TODO: obtain this value from Gtk, Glib, or Cairo.
cairoGetDPI :: Cairo.Render Double
cairoGetDPI = return 96.0

--cairoGetMinFontExtents :: Cairo.Render Cairo.FontExtents
--cairoGetMinFontExtents = do
--  Cairo.save
--  Cairo.selectFontFace ("monospace" :: Strict.Text) Cairo.FontSlantNormal Cairo.FontWeightNormal
--  dpi <- cairoGetDPI
--  Cairo.setFontSize (dpi / 12.0)
--  Cairo.fontExtents
--    <* Cairo.restore

cairoSetFontStyle :: FontStyle -> Cairo.Render ()
cairoSetFontStyle fs = do
  dpi <- cairoGetDPI
  let fontsizemin = dpi / 12.0
  Cairo.setFontSize $ fontsizemin * realToFrac (fontSizeMultiple $ fs ^. fontSize)
  Cairo.selectFontFace ("monospace" :: Strict.Text)
    (if fs ^. fontItalic then Cairo.FontSlantItalic else Cairo.FontSlantNormal )
    (if fs ^. fontBold   then Cairo.FontWeightBold  else Cairo.FontWeightNormal)

cairoArray :: (Int -> Int -> Cairo.SurfaceData Int Word32 -> IO a) -> Cairo.Render a
cairoArray f = Cairo.withTargetSurface $ \ surface -> do
  w <- Cairo.imageSurfaceGetWidth surface
  h <- Cairo.imageSurfaceGetHeight surface
  liftIO $ Cairo.imageSurfaceGetPixels surface >>= f w h

pointToInt :: Int -> Int -> Point2D RealApprox -> Int
pointToInt w _h pt = let (x, y) = (round <$> pt) ^. pointXY in y*w + x

-- | The implementation of 'Happlets.Draw.getPoint' for the 'Cairo.Render' function type.
cairoGetPoint :: Point2D RealApprox -> Cairo.Render Color
cairoGetPoint pt = cairoArray $ \ w h surfaceData ->
  liftIO $ set32BitsARGB <$> readArray surfaceData (pointToInt w h pt)

-- | Call this function at least once before calling 'cairoSetPoint'. Note that if you use a
-- 'CairoRender' function type and the ordinary 'Happlets.Draw.setPoint' function in the
-- "Happlets.Draw" module, this function never needs to be called, the internal state of the
-- 'CairoRender' function tracks when to flush and when to invalidate.
cairoFlush :: Cairo.Render ()
cairoFlush = Cairo.withTargetSurface Cairo.surfaceFlush

-- | Force a single pixel at a given location to change to the given color.
cairoSetPoint :: Point2D RealApprox -> Color -> Cairo.Render ()
cairoSetPoint pt = get32BitsARGB >>> \ w32 -> cairoArray $ \ w h surfaceData ->
  liftIO $ writeArray surfaceData (pointToInt w h pt) w32

-- | Call this function at least once after you have finished a series of calls to 'cairoSetPoint'
-- but before any calls to any other cairo functions. Note that if you use a 'CairoRender' function
-- type and the ordinary 'Happlets.Draw.setPoint' function in the "Happlets.Draw" module, this
-- function never needs to be called, the internal state of the 'CairoRender' function tracks when
-- to flush and when to invalidate.
cairoInvalidate :: Cairo.Render ()
cairoInvalidate = Cairo.withTargetSurface Cairo.surfaceMarkDirty

-- | Use the Happlets-native color data type 'Happlets.Draw.Color.Color' to set the Cairo
-- "source" color in the Cairo context.
cairoSetColor :: Color -> Cairo.Render ()
cairoSetColor = unpackRGBA32Color >>> \ (r,g,b,a) -> Cairo.setSourceRGBA r g b a

-- | Push the graphics context by calling 'Cairo.save' before evaluating a given 'Cario.Render'
-- function. When evaluation completes, pop the graphics context by calling 'Cairo.restore'.
cairoPreserve :: CairoRender a -> CairoRender a
cairoPreserve f = cairoRender Cairo.save >> f <* cairoRender Cairo.restore

-- | Move the position of the cairo graphics context "pen" object.
cairoMoveTo :: Point2D RealApprox -> Cairo.Render ()
cairoMoveTo = uncurry Cairo.moveTo . view pointXY . fmap unwrapRealApprox

-- | Using the cairo graphics context current color, and the position of the "pen" object, draw a
-- line from the current pen position to the given point.
cairoLineTo :: Point2D RealApprox -> Cairo.Render ()
cairoLineTo = uncurry Cairo.lineTo . view pointXY . fmap unwrapRealApprox

-- | Draw a single line of the given color. This will also draw the line caps at the start and end
-- points.
cairoDrawLine :: LineColor -> LineWidth -> Line2D RealApprox -> Cairo.Render ()
cairoDrawLine color width line = do
  cairoSetColor color
  Cairo.setLineCap Cairo.LineCapRound
  Cairo.setLineWidth $ unwrapRealApprox width
  cairoMoveTo $ line ^. line2DHead
  cairoLineTo $ line ^. line2DTail
  Cairo.stroke

-- | Similar to 'cairoDrawLine' but draws multiple line segments, each next segment beginning where
-- the previous segment ended. The line is drawn with the given color. Only two line caps are drawn:
-- one at the first given point and one at the last given point in the list of points.
cairoDrawPath :: LineColor -> LineWidth -> [Point2D RealApprox] -> Cairo.Render ()
cairoDrawPath color width =
  let run a ax = do
        cairoSetColor color
        Cairo.setLineCap Cairo.LineCapRound
        Cairo.setLineWidth $ unwrapRealApprox width
        Cairo.setLineJoin Cairo.LineJoinMiter
        cairoMoveTo a
        forM_ ax cairoLineTo
        Cairo.stroke
  in  \ case { [] -> return (); [a] -> run a [a]; a:ax -> run a ax; }

cairoDrawRect :: LineColor -> LineWidth -> FillColor -> Rect2D RealApprox -> Cairo.Render ()
cairoDrawRect lineColor width fillColor rect = do
  cairoSetColor fillColor
  cairoMoveTo $ rect ^. rect2DHead
  let (head, tail) = (unwrapRealApprox <$> canonicalRect2D rect) ^. rect2DPoints
  let (x, y) = head ^. pointXY
  let (w, h) = (tail - head) ^. pointXY
  Cairo.rectangle x y w h
  Cairo.fill
  when (width > 0.0) $ do
    cairoSetColor lineColor
    Cairo.setLineWidth $ unwrapRealApprox width
    Cairo.setLineJoin Cairo.LineJoinMiter
    Cairo.rectangle x y w h
    Cairo.stroke

-- | This is a helpful function you can use for your 'Happlet.Control.controlRedraw' function to clear
-- the window with a background color, given by the four 'Prelude.Double' parameters for Red, Green,
-- Blue, and Alpha (in that order).
cairoClearCanvas :: Double -> Double -> Double -> Double -> Cairo.Render ()
cairoClearCanvas r g b a = do
  logIO <- mkLogger "cairoClearCanvas"
  liftIO $ logIO _drawevt $ unwords $ show <$> [r,g,b,a]
  op <- Cairo.getOperator
  Cairo.setOperator Cairo.OperatorSource
  Cairo.setSourceRGBA r g b a
  Cairo.paint
  Cairo.setOperator op

--cairoSelectFont :: SelectFont -> GtkGUI model (Maybe FontExtents)
--cairoSelectFont font0 = runGtkStateGUI $ evalCairo $ do
--  let loop (bold,italic,oblique,syms) = \ case
--        []   -> (bold, italic, oblique, Strict.unwords $ syms [])
--        a:ax -> let w = Strict.toLower a in ax & case w of
--          w | w == Strict.pack "bold"    -> loop (bold + 1, italic, oblique, syms)
--          w | w == Strict.pack "italic"  -> loop (bold, italic + 1, oblique, syms)
--          w | w == Strict.pack "oblique" -> loop (bold, italic, oblique + 1, syms)
--          _                  -> loop (bold, italic, oblique, syms . (a :))
--  let zero = 0 :: Int
--  let one = 1 :: Int
--  let (bold,italic,oblique,font) = loop (0, 0, 0, id) $ Strict.words font0
--  if italic > zero && oblique > zero || bold > one || italic > one || oblique > one
--   then return Nothing
--   else do
--    Cairo.selectFontFace font
--      (if italic > 0 then Cairo.FontSlantItalic else
--       if oblique > 0 then Cairo.FontSlantOblique else Cairo.FontSlantNormal)
--      (if bold > 0 then Cairo.FontWeightBold else Cairo.FontWeightNormal)
--    ext <- Cairo.fontExtents
--    return $ Just FontExtents
--      { fontExtentsAscent      = RealApprox $ Cairo.fontExtentsAscent ext
--      , fontExtentsDescent     = RealApprox $ Cairo.fontExtentsDescent ext
--      , fontExtentsHeight      = RealApprox $ Cairo.fontExtentsHeight ext
--      , fontExtentsMaxXadvance = RealApprox $ Cairo.fontExtentsMaxXadvance ext
--      , fontExtentsMaxYadvance = RealApprox $ Cairo.fontExtentsMaxYadvance ext
--      }

----------------------------------------------------------------------------------------------------

instance Managed GtkWindow where
  windowVisible = gtkWindowVisible
  visibleEvents =
    installEventHandler _winevt "visibleEvents" visibilityReaction $ \ logIO sel env next -> do
      let win = gtkWindow env
      logIO (_winevt<>sel) "Glib.on Gtk.visibilityNotifyEvent"
      vis <- Glib.on win Gtk.visibilityNotifyEvent $
        Gtk.eventVisibilityState >>=
        liftIO . next . not . (== Gtk.VisibilityFullyObscured) >>
        return False
      return $ do
        logIO _winevt "Glib.signalDisconnect Gtk.visibilityNotifyEvent"
        Glib.signalDisconnect vis
  focusEvents   =
    installEventHandler _winevt "focusEvents" focusReaction $ \ logIO sel env next -> do
      let win = gtkWindow env
      logIO (_winevt<>sel) "Glib.on Glib.focusInEvent"
      focin  <- Glib.on win Gtk.focusInEvent  $ liftIO (next True)  >> return False
      logIO (_winevt<>sel) "Glib.on Glib.focusOutEvent"
      focout <- Glib.on win Gtk.focusOutEvent $ liftIO (next False) >> return False
      return $ do
        logIO (_winevt<>sel) "Glib.signalDisconnect (Glib.on Glib.focusInEvent)"
        Glib.signalDisconnect focin
        logIO (_winevt<>sel) "Glib.signalDisconnect (Glib.on Glib.focusOutEvent)"
        Glib.signalDisconnect focout

instance CanResize GtkWindow where
  resizeEvents =
    installEventHandler _winevt "resizeEvents" resizeReaction(\ _ _ _ _ -> return $ pure ())

data AnimationThreadControl
  = AnimationThreadControl
    { animationThreadAlive :: !Bool
    , animationInitTime    :: !UTCTime
    }

instance CanAnimate GtkWindow where
  animationIsRunning = runGtkStateGUI _animevt "animationIsRunning" $
    gets theAnimatorThread <&> \ case { Disconnected -> False; _ -> True; }
  stepFrameEvents react = do
    flip (installEventHandler _animevt "stepFrameEvents" animatorThread) react $
      \ logIO sel env next -> do
          let rate = env & theAnimationFrameRate . currentConfig
          t0    <- getCurrentTime
          t0ref <- newIORef AnimationThreadControl
            { animationThreadAlive = True
            , animationInitTime    = t0
            }
          logIO (_animevt<>sel) "Glib.on timeout"
          frame <- flip Glib.timeoutAdd (min 200 $ floor (1000.0 / rate)) $ do
            (AnimationThreadControl
              {animationThreadAlive=alive
              ,animationInitTime=t0
              }) <- readIORef t0ref
            when alive $ diffUTCTime <$> getCurrentTime <*> pure t0 >>= next . realToFrac
            animationThreadAlive <$> readIORef t0ref
          --
          -- TODO: the 'next' function needs to be evaluated here at time 0, becuase 'timeoutAdd'
          -- does not call it until after the first time step interval has passed. For slow
          -- animations, this will cause a noticable delay between the time the 'stepFrameEvents'
          -- function is evaluated and the first frame callback is evaluated.
          return $ do
            logIO (_animevt<>sel) "Glib.signalDisconnect (Glib.on timeout)"
            modifyIORef t0ref $ \ ctrl -> ctrl{ animationThreadAlive = False }
            Glib.timeoutRemove frame

instance CanKeyboard GtkWindow where
  keyboardEvents = installEventHandler _keyevt "keyboardEvents" keyHandler $ \ logIO sel env next -> do
    -- Event boxes do not capture key events for some reason. Key handlers must be installed into
    -- the window widget itself.
    let box = gtkWindow env
    logIO (_keyevt<>sel) "Glib.on Gtk.keyReleaseEvent"
    press   <- Glib.on box Gtk.keyReleaseEvent $
      handleKey False >>= liftIO . next >> return False
    logIO (_keyevt<>sel) "Glib.on Gtk.keyPressEvent"
    release <- Glib.on box Gtk.keyPressEvent $
      handleKey True  >>= liftIO . next >> return False
    return $ do
      logIO (_keyevt<>sel) "Glib.signalDisconnect [Gtk.keyPressEvent, Gtk.keyReleaseEvent]"
      Glib.signalDisconnect press
      Glib.signalDisconnect release

instance CanMouse GtkWindow where
  providedMouseDevices = return []
  mouseEvents = \ case
    MouseButton -> installEventHandler _mousevt "mouseEvents MouseButton" mouseHandler $
      \ logIO sel env next -> do
          let box = env ^. gtkEventBox
          logIO (_mousevt<>sel) $ "Glib.on Gtk.buttonPressEvent"
          press <- Glib.on box Gtk.buttonPressEvent $
            handleMouse True >>= liftIO . next >> return False
          release <- Glib.on box Gtk.buttonReleaseEvent $
            handleMouse False >>= liftIO . next >> return False
          return $ do
            logIO (_mousevt<>sel) $ "Glib.signalDisconnect Gtk.buttonPressEvent"
            Glib.signalDisconnect press
            Glib.signalDisconnect release
    MouseDrag   -> installEventHandler _mousevt "mouseEvents MouseDrag" mouseHandler $
      \ logIO sel env next -> do
          let box = env ^. gtkEventBox
          logIO (_mousevt<>sel) $ "Glib.on Gtk.buttonPressEvent"
          press <- Glib.on box Gtk.buttonPressEvent $
            handleMouse True >>= liftIO . next >> return False
          logIO (_mousevt<>sel) "Glib.on Gtk.buttonReleaseEvent"
          release <- Glib.on box Gtk.buttonReleaseEvent $
            handleMouse False >>= liftIO . next >> return False
          logIO (_mousevt<>sel) "Glib.on Gtk.motionNotifyEvent"
          button <- Glib.on box Gtk.motionNotifyEvent $
            handleCursor True >>= liftIO . next >> return False
          logIO (_mousevt<>sel) "Gtk.widgetAddEvents [Gtk.ButtonMotionMask]"
          Gtk.widgetAddEvents box [Gtk.ButtonMotionMask]
          return $ do
            logIO (_mousevt<>sel) $
              "Glib.signalDisconnect [Gtk.buttonPressEvent, Gtk.buttonReleaseEvent]"
            Glib.signalDisconnect press
            Glib.signalDisconnect release
            Glib.signalDisconnect button
            -- Gtk has an assertion which disallows calling 'Gtk.widgetDelEvents' while a widget is
            -- realized. In order to remove the event mask, we must first remove the widget from the
            -- window container, then add it back again.
            logIO (_mousevt<>sel) "Gtk.containerRemove window eventBox"
            Gtk.containerRemove (gtkWindow env) box
            logIO (_mousevt<>sel) "Gtk.widgetDelEvents eventBox [Gtk.ButtonMotionMask]"
            Gtk.widgetDelEvents box [Gtk.ButtonMotionMask]
            logIO (_mousevt<>sel) "Gtk.containerAdd window eventBox"
            Gtk.containerAdd (gtkWindow env) box
    MouseAll    -> installEventHandler _mousevt "mouseEvents MouseAll" mouseHandler $
      \ logIO sel env next -> do
          let box = env ^. gtkEventBox
          button  <- newIORef False
          logIO (_mousevt<>sel) $ "Glib.on Gtk.buttonPressEvents"
          press   <- Glib.on box Gtk.buttonPressEvent   $
            liftIO (writeIORef button True) >>
            handleMouse True  >>= liftIO . next >> return False
          logIO (_mousevt<>sel) $ "Glib.on Gtk.buttonReleaseEvent"
          release <- Glib.on box Gtk.buttonReleaseEvent $
            liftIO (writeIORef button False) >>
            handleMouse False >>= liftIO . next >> return False
          logIO (_mousevt<>sel) $ "Glib.on Gtk.motionNotifyEvent"
          motion  <- Glib.on box Gtk.motionNotifyEvent  $
            liftIO (readIORef button) >>=
            handleCursor >>= liftIO . next >> return False
          logIO (_mousevt<>sel) $
            "Gtk.widgetAddEvents [Gtk.PointerMotionMask, Gtk.ButtonMotionMask]"
          Gtk.widgetAddEvents box [Gtk.PointerMotionMask, Gtk.ButtonMotionMask]
          return $ do
            logIO (_mousevt<>sel) $
              "Glib.signalDisconnect [Gtk.buttonPressEvent, Gtk.buttonReleaseEvent, Gtk.motionNotifyEvent]"
            Glib.signalDisconnect press
            Glib.signalDisconnect release
            Glib.signalDisconnect motion
            -- Gtk has an assertion which disallows calling 'Gtk.widgetDelEvents' while a widget is
            -- realized. In order to remove the event mask, we must first remove the widget from the
            -- window container, then add it back again.
            logIO (_mousevt<>sel) $ "Gtk.containerRemove window eventBox"
            Gtk.containerRemove (gtkWindow env) box
            logIO (_mousevt<>sel) $
              "Gtk.widgetDelEvents eventBox [Gtk.PointerMotionMask, Gtk.ButtonMotionMask]"
            Gtk.widgetDelEvents box [Gtk.PointerMotionMask, Gtk.ButtonMotionMask]
            logIO (_mousevt<>sel) $ "Gtk.containerAdd window eventBox"
            Gtk.containerAdd (gtkWindow env) box

-- | This function enables an event handler, and takes a continuation which will be evaluated by
-- this function call to do the low-level work of install the event handler into the Gtk
  -- window. This function also does the work of taking a 'Happlets.GUI.GUI' function to be evaluated
-- on each event, converting this function to a 'ConnectReact' function, and installing it into the
-- 'GtkWindow's internal state so that it can be actually evaluated every time the low-level event
-- handler is evaluated. The result is a function that can be used to instantiate any of the
-- "Happlets.GUI" module's event handler classes.
installEventHandler
  :: Show event
  => DebugTag
  -> String
  -> Lens' GtkWindowState (ConnectReact event)
  -> (Log IO -> DebugTag -> GtkWindowState -> (event -> IO ()) -> IO (IO ()))
  -> (event -> GtkGUI model ())
  -> GtkGUI model ()
installEventHandler sel what connectReact install react = do
  logIO <- mkLogger what
  happ  <- askHapplet
  let dbgmsg = "installEventHandler "++what
  runGtkStateGUI sel dbgmsg $ do
    env <- get
    forceDisconnect logIO sel what connectReact
    disconnect <- liftIO $ do
      logIO sel $ "-- install "++what++"event handler"
      install logIO sel env $ \ event -> do
        lockGtkWindow logIO sel (GtkUnlockedWin $ thisWindow env) $
          evalConnectReact logIO sel what connectReact event
    liftIO $ logIO sel $ what ++ " -- now enabled"
    connectReact .= ConnectReact
      { doReact = logSubGtk logIO sel ("gtkRunHapplet "++what) .
          gtkRunHapplet happ . react >=> \ guiCont ->
            logSubGtk logIO sel (unwords ["checkGUIContinue", what, show guiCont]) $
              checkGUIContinue logIO sel what connectReact guiCont
      , doDisconnect = liftIO (logIO sel $ "disconnect "++what) >> disconnect
      }

handleMouse :: Pressed -> Gtk.EventM Gtk.EButton Mouse
handleMouse upDown = do
  logIO <- mkLogger "handleMouse"
  (x, y) <- Gtk.eventCoordinates
  button <- getMouseButton
  mods   <- packGtkModifiers <$> Gtk.eventModifierAll
  let evt = Mouse "" upDown mods button (V2 (round x) (round y))
  liftIO $ logIO _mousevt $ show evt
  return evt

handleCursor :: Pressed -> Gtk.EventM Gtk.EMotion Mouse
handleCursor upDown = do
  logIO <- mkLogger "handleCursor"
  (x, y) <- Gtk.eventCoordinates
  mods   <- packGtkModifiers <$> Gtk.eventModifierAll
  let evt = Mouse "" upDown mods MotionOnly (V2 (round x) (round y))
  liftIO $ logIO _mousevt $ show evt
  return evt

handleKey :: Pressed -> Gtk.EventM Gtk.EKey Keyboard
handleKey upDown = do
  logIO  <- mkLogger "handleKey"
  keyval  <- Gtk.eventKeyVal
  keyName <- Gtk.eventKeyName
  mods    <- packGtkModifiers <$> Gtk.eventModifierAll
  ch      <- liftIO $ Gtk.keyvalToChar keyval
  evt     <- liftIO $ case ch of
    Just ch -> return $ Keyboard upDown mods (CharKey ch)
    Nothing -> do
      let sym = Keyboard upDown mods (SymbolKey keyName)
      return $ case Strict.unpack keyName of
        ""      -> RawKey upDown mods keyval
        'F':num -> case readsPrec 0 num of
          [(i, "")] -> Keyboard upDown mods (FuncKey i)
          _         -> sym
        _       -> case Map.lookup keyName keyNameMap of
          Nothing                   -> sym
          Just (moreMods, keyPoint) -> Keyboard upDown (mods <> moreMods) keyPoint
  liftIO $ logIO _keyevt $ show evt
  return evt

getMouseButton :: Gtk.EventM Gtk.EButton MouseButton
getMouseButton = Gtk.eventButton <&> \ case
  Gtk.LeftButton    -> LeftClick
  Gtk.MiddleButton  -> MiddleClick
  Gtk.RightButton   -> RightClick
  Gtk.OtherButton{} -> SideClick

packGtkModifiers :: [Gtk.Modifier] -> ModifierBits
packGtkModifiers = packModifiers .
  ( (=<<) $ \ case
      Gtk.Shift   -> [ShiftKey]
      Gtk.Lock    -> [CapsLockKey]
      Gtk.Control -> [CtrlKey]
      Gtk.Alt     -> [AltKey]
      Gtk.Alt2    -> [CommandKey]
      Gtk.Super   -> [Super1Key]
      Gtk.Hyper   -> [Super2Key]
      _           -> []
  )

keyNameMap :: Map.Map Strict.Text (ModifierBits, KeyPoint)
keyNameMap = let k nm key mod = (Strict.pack nm, (packModifiers mod, key)) in Map.fromList
  [ k "Tab"               TabKey         []
  , k "Return"            ReturnKey      []
  , k "BackSpace"         BackSpaceKey   []
  , k "Escape"            EscapeKey      []
  , k "Delete"            DeleteKey      []
  , k "Up"                UpArrowKey     []
  , k "Down"              DownArrowKey   []
  , k "Left"              LeftArrowKey   []
  , k "Right"             RightArrowKey  []
  , k "Enter"             EnterKey       []
  , k "Home"              HomeKey        []
  , k "End"               EndKey         []
  , k "Menu"              MenuKey        []
  , k "Insert"            InsertKey      []
  , k "PageUp"            PageUpKey      []
  , k "PageDown"          PageDownKey    []
  , k "Pause"             PauseKey       []
  , k "Break"             BreakKey       []
  , k "SysRqKey"          SysRqKey       []
  , k "Print_Screen"      PrintScreenKey []
  , k "Scroll_Lock"       ScrollLockKey  []
  , k "Num_Lock"          NumLockKey     []
  , k "Kanji"             KanjiKey       []
  , k "Henkan"            HenkanKey      []
  , k "Muhenkan"          MuhenkanKey    []
  , k "Zenkaku_Hankaku"   ZenkakuHankakuKey   []
  , k "Hiragana_Katakana" HiraganaKatakanaKey []
  , k "Shift_L"           ModifierOnly   [LeftShiftKey]
  , k "Shift_R"           ModifierOnly   [RightShiftKey]
  , k "Alt_L"             ModifierOnly   [LeftAltKey]
  , k "Alt_R"             ModifierOnly   [RightAltKey]
  , k "Command_L"         ModifierOnly   [LeftCommandKey]
  , k "Command_R"         ModifierOnly   [RightCommandKey]
  , k "Control_L"         ModifierOnly   [LeftCtrlKey]
  , k "Control_R"         ModifierOnly   [RightCtrlKey]
  , k "Super_L"           ModifierOnly   [LeftSuper1Key]
  , k "Super_R"           ModifierOnly   [RightSuper1Key]
  ]

--screenMasksToGdkRegion :: [ScreenMask] -> (Int, Int) -> IO Gtk.Region
--screenMasksToGdkRegion masks (w, h) = do
--  logGUI <- mkLogger "screenMasksToGdkRegion" True
--  let center x w = let (a, r) = divMod w 2 in (x - a, x + a + r)
--  let int (SampCoord i) = fromIntegral i :: Int
--  let rect xa' ya' xb' yb' =
--        let xa = int $ min xa' xb'
--            xb = int $ max xa' xb'
--            ya = int $ min ya' yb'
--            yb = int $ max ya' yb'
--            w  = xb - xa
--            h  = yb - ya
--        in  Gtk.Rectangle xa ya w h
--  region <- Gtk.regionNew
--  let union = Gtk.regionUnion region
--  let loop = \ case
--        []         -> return ()
--        mask:masks -> case mask of
--          ScreenAll  ->
--            Gtk.regionRectangle (Gtk.Rectangle 0 0 w h) >>= union
--          ScreenRect (V2 xa ya) (V2 xb yb) -> do
--            logGUI $ "Gtk.regionRectangle ScreenRect{ xa="++
--              show xa++", ya="++show ya++", xb="++show xb++", yb="++show yb++" }"
--            Gtk.regionRectangle (rect xa ya xb yb) >>= union
--            loop masks
--          ScreenOval (V2 x y) w h -> do
--            let (xa, xb) = center x w
--                (ya, yb) = center y h
--            -- TODO: Gtk.regionPolygon with approximate oval shape if greater than a certain size.
--            logGUI $ "Gtk.regionRectangle " ++ ("ScreenOval{ x="++
--              show xa++" y="++show ya++" xb="++show xb++" yb="++show yb++" }")
--            Gtk.regionRectangle (rect xa ya xb yb) >>= union
--            loop masks
--          ScreenTriangle (V2 xa ya) (V2 xb yb) (V2 xc yc) -> do
--            let points = [(int xa, int ya), (int xb, int yb), (int xc, int yc)]
--            logGUI $ "Gtk.regionPolygon "++show points
--            Gtk.regionPolygon points Gtk.WindingRule >>= union
--            loop masks
--          ScreenVertical   x lo hi ->
--            Gtk.regionRectangle (rect x lo (x + 1) hi) >>= union >> loop masks
--          ScreenHorizontal y lo hi ->
--            Gtk.regionRectangle (rect lo y hi (y + 1)) >>= union >> loop masks
--  loop masks >> return region

---- | Queues a redraw on the area of the window covered by the given list of
---- 'Happlets.Redraw.ScreenMask's.
--updateRegions :: Gtk.DrawWindow -> [ScreenMask] -> IO ()
--updateRegions canvas masks = if null masks then return () else do
--  logGUI <- mkLogger "updateRegions" False
--  region <- Gtk.drawableGetSize canvas >>= screenMasksToGdkRegion masks
--  logGUI "Gtk.drawWindowInvalidateRegion"
--  Gtk.drawWindowInvalidateRegion canvas region True

#if ! USE_CAIRO_SURFACE_BUFFER
-- | Blit a 'Gtk.Pixmap' to some drawable canvas.
gdkBlit :: Gtk.DrawableClass canvas => canvas -> Gtk.Pixmap -> PixCoord -> IO ()
--gdkBlit :: Gtk.DrawableClass canvas => canvas -> Gtk.Pixmap -> PixCoord -> [ScreenMask] -> IO ()
gdkBlit canvas pixmap (V2 dx dy)  = do
  logIO <- mkLogger "gdkBlit"
  --region <- screenMasksToGdkRegion masks (fromIntegral dx, fromIntegral dy)
  (w, h) <- Gtk.drawableGetSize canvas
  -- TODO: This creates an update region for the whole window. This needs to be made more efficient,
  -- such that only the minimal updated region is redrawn.
  region <- Gtk.regionNew
  Gtk.regionRectangle (Gtk.Rectangle 0 0 w h) >>= Gtk.regionUnion region
  logIO _drawevt $ "Gtk.gcNewWithValues " ++ show dx ++ ' ' : show dy
  gc <- Gtk.gcNewWithValues canvas $ Gtk.newGCValues
    { Gtk.tile = Just pixmap
    , Gtk.fill = Gtk.Tiled
    , Gtk.tsXOrigin = fromIntegral dx
    , Gtk.tsYOrigin = fromIntegral dy
    }
  Gtk.gcSetClipRegion gc region
  logIO _drawevt $ "Gtk.drawRectangle 0 0 " ++ show w ++ ' ' : show h
  Gtk.drawRectangle canvas gc True 0 0 w h 
  --rects  <- Gtk.regionGetRectangles region
  --forM_ rects $ \ rect@(Gtk.Rectangle x y w h) -> do
  --  logGUI $ "Gtk.drawRectangle canvas gc True (" ++ show rect ++ ")"
  --  Gtk.drawRectangle canvas gc True (x + fromIntegral dx) (y + fromIntegral dy) w h
#endif

----------------------------------------------------------------------------------------------------

-- | A 'GtkDrawing' is a function that produces a 'Diagrams.Core.Types.Diagram' from a
-- 'Diagrams.BoundingBox.BoundingBox'. Use the 'Diagrams.BoundingBox.BoundingBox' information to
-- inform the placement and scale of your diagram.
type GtkCairoDiagram = BoundingBox V2 Double -> Diagram Cairo

-- | This data type contains a pointer to an image buffer in memory, and also a function used to
-- perform some drawing to the pixel values.
data GtkImage
  = GtkImage
    {
#if USE_CAIRO_SURFACE_BUFFER
      gtkCairoSurfaceMVar :: MVar Cairo.Surface
#else
      gtkPixmapMVar :: MVar Gtk.Pixmap
#endif
    }

instance CanBufferImages GtkWindow GtkImage CairoRender where
#if USE_CAIRO_SURFACE_BUFFER
  newImageBuffer size@(V2 w h) draw = runGtkStateGUI _drawevt "newImageBuffer" $ do
    rendst <- use cairoRenderState
    (a, rendst, img) <- liftIO $ do
      logIO <- mkLogger "newImageBuffer"
      logIO _drawevt $ unwords ["Cairo.createImageSurface Cairo.FormatARGB32", show w, show h]
      surface <- Cairo.createImageSurface Cairo.FormatARGB32 (fromIntegral w) (fromIntegral h)
      (a, rendst) <- logSubIO logIO _drawevt "Cairo.renderWith newImageBuffer" $
        Cairo.renderWith surface $ runCairoRender size rendst draw
      mvar <- newMVar surface
      return (a, rendst, GtkImage{gtkCairoSurfaceMVar=mvar})
    cairoRenderState .= rendst
    return (a, img)

  resizeImageBuffer (GtkImage{gtkCairoSurfaceMVar=mvar}) size@(V2 w h) draw =
    runGtkStateGUI _drawevt ("resizeImageBuffer ("++show size++")") $ do
      rendst <- use cairoRenderState
      (a, rendst) <- liftIO $ do
        logIO <- mkLogger "resizeImageBuffer"
        logModMVar logIO _drawevt "gtkCairoSurfaceMVar" mvar $ \ surface -> do
          oldDims <- (,)
            <$> Cairo.imageSurfaceGetWidth  surface
            <*> Cairo.imageSurfaceGetHeight surface
          let f (SampCoord a) = fromIntegral a
          surface <- if oldDims == (f w, f h) then return surface else do
            logIO _drawevt $
              unwords ["Cairo.createImageSurface Cairo.FormatARGB32", show w, show h]
            Cairo.createImageSurface Cairo.FormatARGB32 (fromIntegral w) (fromIntegral h)
          (a, rendst) <- logSubIO logIO _drawevt "Cairo.renderWith resizeImageBuffer" $
            Cairo.renderWith surface $ runCairoRender size rendst draw
          return (surface, (a, rendst))
      cairoRenderState .= rendst
      return a

  drawImage (GtkImage{gtkCairoSurfaceMVar=mvar}) draw = runGtkStateGUI _drawevt "drawImage" $ do
    rendst <- use cairoRenderState
    (a, rendst) <- liftIO $ do
      logIO <- mkLogger "drawImage"
      logWithMVar logIO _drawevt "gtkCairoSurfaceMVar" mvar $ \ surface -> do
        size <- V2
          <$> Cairo.imageSurfaceGetWidth  surface
          <*> Cairo.imageSurfaceGetHeight surface
        logSubIO logIO _drawevt "Cairo.renderWith drawImage" $
          Cairo.renderWith surface $ runCairoRender (sampCoord <$> size) rendst draw
    cairoRenderState .= rendst
    return a

  blitImage (GtkImage{gtkCairoSurfaceMVar=mvar}) offset = runGtkStateGUI _drawevt "blitImage" $ do
    logIO <- mkLogger "blitImage"
    liveMVar <- gets gtkWindowLive
    liftIO $ logWithMVar logIO _drawevt "gtkWindowLive" liveMVar $ \ liveEnv ->
      logSubIO logIO _drawevt "modifyMVar gtkCairoSurfaceMVar" $
        logWithMVar logIO _drawevt "gtkCairoSurfaceMVar" mvar $ \ surface ->
          logSubIO logIO _drawevt "Cairo.renderWith blitImage" $
            Cairo.renderWith (theCairoSurface liveEnv) $ do
              let (V2 x y) = realToFrac <$> offset
              liftIO $ logIO _drawevt $
                unwords ["Cairo.setSourceSurface", show x, show y, ">> Cairo.paint"]
              Cairo.setOperator Cairo.OperatorSource
              Cairo.setSourceSurface surface x y
              Cairo.paint

  blitImageTo (GtkImage{gtkCairoSurfaceMVar=src}) (GtkImage{gtkCairoSurfaceMVar=targ}) offset =
    runGtkStateGUI _drawevt "blitImageTo" $ liftIO $
      if src == targ then error "cannot blit image to itself" else do
        logIO <- mkLogger "blitImageTo"
        logWithMVar logIO _drawevt "blitSource" src $ \ src ->
          logWithMVar logIO _drawevt "blitTarget" targ $ \ targ ->
            logSubIO logIO _drawevt "Cairo.renderWith blitImageTo" $ Cairo.renderWith targ $ do
              let (V2 x y) = realToFrac <$> offset
              liftIO $ logIO _drawevt $ unwords
                ["Cairo.setSourceSurface", show x, show y, ">> Cairo.paint"]
              Cairo.setOperator Cairo.OperatorSource
              Cairo.setSourceSurface src x y
              Cairo.paint

#else
  newImageBuffer  (V2 (SampCoord w) (SampCoord h)) draw = liftIO $ do
    logIO <- mkLogger "newImageBuffer"
    logIO _drawevt $ unwords ["Gtk.pixmapNew", show w, show h]
    pixmap <- Gtk.pixmapNew (Nothing :: Maybe Gtk.Pixmap)
      (fromIntegral w) (fromIntegral h) (Just 32)
    logSubIO _drawevt "Gtk.renderWithDrawable newImageBuffer" $
      Gtk.renderWithDrawable pixmap $ runCairoRender draw
    mvar <- newMVar pixmap
    return GtkImage
      { gtkPixmapMVar = mvar
      }

  resizeImageBuffer (GtkImage{gtkPixmapMVar=mvar}) (V2 w h) draw = do
    logIO <- mkLogger "resizeImageBuffer"
    liftIO $ logModMVar logIO _drawevt mvar $ \ pixmap -> do
      logIO _drawevt $ unwords ["Gtk.pixmapNew", show w, show h]
      pixmap <- Gtk.pixmapNew (Just pixmap) (fromIntegral w) (fromIntegral h) (Just 32)
      a <- logSubIO _drawevt "Gtk.renderWithDrawable resizeImageBuffer" $
        Gtk.renderWithDrawable pixmap $ runCairoRender draw
      return (pixmap, a)

  drawImage (GtkImage{gtkPixmapMVar=mvar}) draw = do
    logIO <- mkLogger "drawImage"
    liftIO $ logWithMVar logIO _drawevt mvar $
      logIO _drawevt "Gtk.renderWithDrawable drawImage" .
      flip Gtk.renderWithDrawable (runCairoRender draw)

  blitImage (GtkImage{gtkPixmapMVar=mvar}) offset = runGtkStateGUI $ do
    logIO <- mkLogger "blitImage"
    gtkwin <- gets gtkWindow
    canvas <- liftIO $ Gtk.widgetGetDrawWindow gtkwin
    liftIO $ logWithMVar logIO _drawevt mvar $ \ pixmap -> gdkBlit canvas pixmap offset

  blitImageTo (GtkImage{gtkPixmapMVar=src}) (GtkImage{gtkPixmapMVar=targ}) offset = do
    logIO <- mkLogger "blitImageTo"
    liftIO $ logWithMVar logIO _drawevt src $ \ src -> logWithMVar logIO _drawevt targ $ \ targ ->
      gdkBlit targ src offset
#endif

-- | Convert a 'GtkCairoDiagram', which is a type of 'Diagrams.Core.Types.Diagram', and convert it
-- to a Cairo 'Cairo.Render'-ing computation which can be used to set the 'controlView' of the
-- 'Controller'.
gtkCairoDiagram :: GtkCairoDiagram -> V2 Double -> Cairo.Render ()
gtkCairoDiagram diagram size = snd $ renderDia Cairo
  ( CairoOptions
    { _cairoFileName     = ""
    , _cairoSizeSpec     = dims size
    , _cairoOutputType   = RenderOnly
    , _cairoBypassAdjust = True
    }
  ) (diagram $ fromCorners origin (P size))

----------------------------------------------------------------------------------------------------

-- | This is the Happlet back-end 'Happlets.Provider.Provider' which you must pass to
-- 'Happlets.GUI.runGUI' in the @main@ function of your Happlet program.
gtkHapplet :: Provider GtkWindow
gtkHapplet = Provider
  { defaultConfig = Config
      { theConfigErrorsOnLoad      = []
      , theConfigFilePath          = ""
      , theRegisteredAppName       = "Happlet"
      , theWindowTitleBar          = "Happlet"
      , theBackgroundTransparency  = Just 0.9
      , theBackgroundGreyValue     = 1.0
      , theRecommendWindowPosition = (0, 30)
      , theRecommendWindowSize     = (800, 600)
      , theAnimationFrameRate      = gtkAnimationFrameRate
      , willDecorateWindow         = True
      , willQuitOnWindowClose      = False
      , willDeleteWindowOnClose    = False
      }
  , doInitializeGUI         = gtkInit
  , doGUIEventLoopLaunch    = gtkLaunchEventLoop
  , doWindowNew             = gtkNewWindow
  , doWindowDelete          = \ env -> do
      logIO <- mkLogger "doWindowDelete"
      lockGtkWindow logIO _winevt env $ get >>= liftIO . deleteWin
  , doWindowAttach          = gtkAttachHapplet
  }

-- A static variable used to ensure the functions in this module only call 'gtkInit' once.
gtkInitCheck :: MVar Bool
gtkInitCheck = unsafePerformIO $ newMVar False
{-# NOINLINE gtkInitCheck #-}

-- You must call this function prior to all evaluation of any GUI programming. This will call the
-- stateful IO functions that initialize the Gtk+ library. Be sure to call this only once.
gtkInit :: IO ()
gtkInit = do
  alreadyInitd <- readMVar gtkInitCheck
  unless alreadyInitd $ do
    logIO <- mkLogger "gtkInit"
    logIO _setup "Gtk.initGUI"
    Gtk.initGUI >>= mapM_ (logIO _setup)
    logModMVar_ logIO _setup "gtkInitCheck" gtkInitCheck $ return . const True

-- | Launch the Gtk+ main event loop. This function is usually called via the
-- 'Happlets.Initialize.launchGUIEventLoop' function, which is automatically called by the
-- 'Happlets.Initialize.happlet' function.
gtkLaunchEventLoop :: Config -> IO ()
gtkLaunchEventLoop cfg = do
  logIO <- mkLogger "gtkEventLoop"
  let appName = cfg ^. registeredAppName
  unless (Strict.null appName) $ do
    logIO _setup $ "Glib.setProgramName " ++ show appName
    Glib.setProgramName appName
    logIO _setup $ "Glib.setApplicationName " ++ show appName
    Glib.setApplicationName appName
  logIO _setup "Gtk.mainGUI"
  Gtk.mainGUI
