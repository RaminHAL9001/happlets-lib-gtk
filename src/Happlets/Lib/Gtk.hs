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
    cairoClearCanvas, cairoSetColorRGBA32,
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

-- I am still not sure how to manipulate the event masks properly. I do not know whether it is
-- better to capture events by installing mouse and keyboard event handlers into a Gtk+ EventBox, or
-- to install event handlers directly into the Gtk+ Window object. I have encoded both ways of event
-- captuere into this source file in case I need to switch to one or the other.
#define USE_EVENT_BOX            0

----------------------------------------------------------------------------------------------------

import           Happlets
import           Happlets.Draw
import           Happlets.Provider

import           Control.Arrow
import           Control.Concurrent

import           Data.Array.MArray
import           Data.IORef
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Text         as Strict
import           Data.Time.Clock
import           Data.Word

import qualified Graphics.Rendering.Cairo           as Cairo

import qualified Graphics.UI.Gtk.Abstract.Widget    as Gtk
import qualified Graphics.UI.Gtk.Cairo              as Gtk
import qualified Graphics.UI.Gtk.Gdk.Drawable       as Gtk
import qualified Graphics.UI.Gtk.Gdk.DrawWindow     as Gtk
import qualified Graphics.UI.Gtk.Gdk.EventM         as Gtk
import qualified Graphics.UI.Gtk.Gdk.Region         as Gtk
import qualified Graphics.UI.Gtk.Gdk.Screen         as Gtk
import qualified Graphics.UI.Gtk.General.General    as Gtk
import qualified Graphics.UI.Gtk.Windows.Window     as Gtk

#if USE_EVENT_BOX
import           Data.Word
import qualified Graphics.UI.Gtk.Abstract.Container as Gtk
import qualified Graphics.UI.Gtk.Gdk.GC             as Gtk
import qualified Graphics.UI.Gtk.Gdk.Pixmap         as Gtk
import qualified Graphics.UI.Gtk.Misc.EventBox      as Gtk
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

debugThisModule :: Bool
debugThisModule  = False

type LogGUI = String -> IO ()

mkLogger
  :: (Monad m, MonadIO m)
  => String -> Bool -> m LogGUI
mkLogger func enable = return $ if not (debugThisModule && enable)
  then const $ return ()
  else \ msg -> do
    tid <- myThreadId
    traceM $ '[' : show tid ++ "][Happlets.Lib.Gtk." ++ func ++ "] " ++ msg

logModMVar :: LogGUI -> String -> MVar a -> (a -> IO (a, b)) -> IO b
logModMVar logGUI label mvar f = do
  logGUI $ "modifyMVar " ++ label ++ " -- begin"
  b <- modifyMVar mvar f
  logGUI $ "modifyMVar " ++ label ++ " -- end"
  return b

logModMVar_ :: LogGUI -> String -> MVar a -> (a -> IO a) -> IO ()
logModMVar_ logGUI label mvar f = do
  logGUI $ "modifyMVar_ " ++ label ++ " -- begin"
  modifyMVar_ mvar f
  logGUI $ "modifyMVar_ " ++ label ++ " -- end"

logWithMVar :: LogGUI -> String -> MVar a -> (a -> IO b) -> IO b
logWithMVar logGUI label mvar f = do
  logGUI $ "withMVar " ++ label ++ " -- begin"
  b <- withMVar mvar f
  logGUI $ "withMVar " ++ label ++ " -- end"
  return b

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
gtkAnimationFrameRate = 60.0

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
    , theCairoRenderState    :: !CairoRenderState
    , theInitReaction        :: !(ConnectReact PixSize)
    , theResizeReaction      :: !(ConnectReact PixCoord)
    , theVisibilityReaction  :: !(ConnectReact Bool)
    , theFocusReaction       :: !(ConnectReact Bool)
    , theMouseHandler        :: !(ConnectReact Mouse)
    , theCursorHandler       :: !(ConnectReact Mouse)
    , theKeyHandler          :: !(ConnectReact Keyboard)
    , theAnimatorThread      :: !(ConnectReact AnimationMoment)
#if USE_EVENT_BOX
    , gtkEventBox            :: !Gtk.EventBox
#endif
    }

data ConnectReact event
  = Disconnected
  | ConnectReact{ doDisconnect :: GtkState (), doReact :: event -> GtkState Bool }

#if ! USE_EVENT_BOX
gtkEventBox :: GtkWindowState -> Gtk.Window
gtkEventBox = gtkWindow
#endif

evalConnectReact :: ConnectReact event -> event -> GtkState Bool
evalConnectReact = \ case
  Disconnected -> const $ return False
  ConnectReact{doReact=f} -> f

forceDisconnect :: Lens' GtkWindowState (ConnectReact event) -> GtkState ()
forceDisconnect connection = use connection >>= \ case
  ConnectReact{doDisconnect=discon} -> discon >> connection .= Disconnected
  Disconnected -> return ()

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

cursorHandler :: Lens' GtkWindowState (ConnectReact Mouse)
cursorHandler = lens theCursorHandler $ \ a b -> a{ theCursorHandler = b }

keyHandler :: Lens' GtkWindowState (ConnectReact Keyboard)
keyHandler = lens theKeyHandler $ \ a b -> a{ theKeyHandler = b }

animatorThread :: Lens' GtkWindowState (ConnectReact AnimationMoment)
animatorThread = lens theAnimatorThread $ \ a b -> a{ theAnimatorThread = b }

-- Create an image buffer with an alpha channel for a 'Graphics.UI.Gtk.Abstract.Widget.Widget' to
-- be rendered.
mkAlphaChannel :: LogGUI -> Gtk.Widget -> IO ()
mkAlphaChannel logGUI window = do
  logGUI $ "Gtk.widgetGetScreen"
  screen <- Gtk.widgetGetScreen window
  logGUI $ "Gtk.screenGetRGBAColormap"
  colormap <- Gtk.screenGetRGBAColormap screen
  case colormap of
    Nothing       -> do
      logGUI $ "-- Colormap dos NOT support alpha channels"
      return ()
    Just colormap -> do
      logGUI $ "Gtk.widgetSetColorMap"
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
lockGtkWindow :: LogGUI -> GtkWindow -> GtkState a -> IO a
lockGtkWindow logGUI win f = case win of
  GtkLockedWin{} -> error "lockGtkWindow: evaluated on an already locked 'GtkLockedWin' window."
  GtkUnlockedWin mvar -> logModMVar logGUI "GtkUnlockedWin" mvar $
    liftM (\ (a, b) -> (b, a)) . runGtkState f

-- | This function evaluates 'Happlets.GUI.GUI' functions within event handlers. It evaluates to a
-- 'GtkState' function, which means you are required to have first evaluated 'unlockGtkWindow'. This
-- function will then lock the 'Happlets.GUI.Happlet' and then evaluate the 'Happlets.GUI.GUI'
-- function.
gtkRunHapplet
  :: Happlet model -> (GUIContinue -> GtkState ()) -> GtkGUI model void -> GtkState Bool
gtkRunHapplet happ cont f = do
  logGUI <- mkLogger "gtkRunHapplet" True
  env <- get
  liftIO $ logGUI $ "locking Happlet, evaluating GUI function..."
  ((env, keep), _) <- liftIO $ onHapplet happ $ \ model -> do
    logGUI "evalGUI"
    gui <- evalGUI f happ (GtkLockedWin env) model
    let model = theGUIModel gui
    case theGUIWindow gui of
      GtkUnlockedWin{} -> error
        "gtkRunHapplet: evalGUI evaluated on GtkLockedWin but returned GtkUnlockedWin"
      GtkLockedWin env -> do
        logGUI "runGtkState -- evaluate continuation"
        ((), env) <- runGtkState (cont $ theGUIContinue gui) env
        return ((env, guiIsLive gui), model)
  state $ const (keep, env)

-- | This function is intended to be passed as a parameter to 'gtkRunHapplet' by event handling
-- functions which check if the 'Happlets.GUI.GUI' function evaluated to 'Happlets.GUI.disable'.
checkGUIContinue
  :: LogGUI
  -> Lens' GtkWindowState (ConnectReact event)
  -> GUIContinue
  -> GtkState ()
checkGUIContinue logGUI connection = \ case
  GUIHalt      -> forceDisconnect connection
  GUIFail  msg -> do
    liftIO $ logGUI $ "GUI function evaluated to \"fail\" with message:\n  " ++ show msg
    forceDisconnect connection
  GUIContinue  -> return ()

-- | This function should be evaluated from within a 'GtkGUI' function when it is necessary to
-- update the 'GtkWindowState', usually this is for installing or removing event handlers.
runGtkStateGUI :: GtkState a -> GtkGUI model a
runGtkStateGUI f = getGUIState >>= \ gui -> case theGUIWindow gui of
  GtkUnlockedWin{} -> error $ "runGtkStateGUI: " ++
    "Evaluated a GtkState function within a GtkGUI function on a locked GtkWindow."
    -- The 'window' value passed to the 'evalGUI' function must be a 'GtkLockedWin' constructor.
  GtkLockedWin env -> do
    (a, env) <- liftIO $ runGtkState f env
    putGUIState $ gui{ theGUIWindow = GtkLockedWin env }
    return a

-- | Create the window and install the permanent event handlers.
createWin :: Config -> IO GtkWindowState
createWin cfg = do
  logGUI <- mkLogger "createWin" True
  forM_ (configErrorsOnLoad cfg) (hPrint stderr)
  -- new window --
  logGUI $ "Gtk.windowNew"
  window <- Gtk.windowNew
  logGUI $ "Gtk.widgetSetHasWindow"
  Gtk.widgetSetHasWindow window True
  logGUI $ "Gtk.widgetSetAppPaintable"
  Gtk.widgetSetAppPaintable window True
  logGUI $ "Gtk.widgetSetDoubleBuffered window False"
  Gtk.widgetSetDoubleBuffered window False
  when (isJust $ cfg ^. backgroundTransparency) $ mkAlphaChannel logGUI $ Gtk.castToWidget window
  logGUI $ "Gtk.windowSetTypeHint GtkWindowTtypeHintNormal"
  Gtk.windowSetTypeHint window Gtk.WindowTypeHintNormal
  logGUI $ "Gtk.windowSetDefaultSize " ++ show (cfg ^. recommendWindowSize)
  uncurry (Gtk.windowSetDefaultSize window) $ cfg ^. recommendWindowSize
  logGUI $ "Gtk.windowSetDecorated " ++ show (cfg ^. decorateWindow)
  Gtk.windowSetDecorated window $ cfg ^. decorateWindow
#if USE_EVENT_BOX
  logGUI $ "Gtk.eventBoxNew"
  eventBox <- Gtk.eventBoxNew
  logGUI $ "Gtk.eventBoxSetVisibleWindow eventBox false"
  Gtk.eventBoxSetVisibleWindow eventBox False
  logGUI $ "Gtk.containerAdd window eventBox"
  Gtk.containerAdd window eventBox
#endif
  this <- newEmptyMVar
  live <- newEmptyMVar
  let env = GtkWindowState
        { currentConfig            = cfg
        , thisWindow               = this
        , gtkWindowLive            = live
        , gtkWindow                = window
        , theCairoRenderState      = CairoRenderState
            { cairoKeepWinSize           = V2 0 0
            --  , theMinFontExtents          = Nothing
            , theCairoRenderMode         = VectorMode
            , theCairoScreenPrinterState = screenPrinterState
            }
        , theInitReaction          = Disconnected
        , theResizeReaction        = Disconnected
        , theVisibilityReaction    = Disconnected
        , theFocusReaction         = Disconnected
        , theMouseHandler          = Disconnected
        , theCursorHandler         = Disconnected
        , theKeyHandler            = Disconnected
        , theAnimatorThread        = Disconnected
#if USE_EVENT_BOX
        , gtkEventBox              = eventBox
#endif
        }
  ((), env) <- flip runGtkState env $ do
    installExposeEventHandler
    installDeleteEventHandler
    installInitEventHandler
  putMVar this env
  return env

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
  logGUI <- mkLogger "installExposeEventHandler" True
  env <- get
  liftIO $ do
    --logGUI $ "Gtk.widgetAddEvents env [Gtk.StructureMask]"
    --Gtk.widgetAddEvents (gtkWindow env) [Gtk.StructureMask, Gtk.ExposureMask]
    --------------------------------------- Expose Event -----------------------------------
    logGUI "Glib.on Gtk.exposeEvent -- for init handler"
    initExposeEventMVar <- newEmptyMVar
    (>>= (putMVar initExposeEventMVar)) $ Glib.on (gtkWindow env) Gtk.exposeEvent $ do
      logGUI <- mkLogger "initExposeEventCallback" True
      liftIO $ do
        logGUI "Gtk.eventWindow >>= putMVar gtkDrawWindowMVar"
        takeMVar initExposeEventMVar >>= Glib.signalDisconnect
        logGUI "Glib.on window Gtk.exposeEvent"
        Glib.on (gtkWindow env) Gtk.exposeEvent $ do
          logGUI <- mkLogger "exposeEventCallback" False
          canvas <- Gtk.eventWindow
          region <- Gtk.eventRegion
          liftIO $ logWithMVar logGUI "gtkWindowLive" (gtkWindowLive env) $ \ livest -> do
#if USE_CAIRO_SURFACE_BUFFER
            Gtk.renderWithDrawable canvas $ do
              let surf = theCairoSurface livest
              --w <- Cairo.imageSurfaceGetWidth  surf
              --h <- Cairo.imageSurfaceGetHeight surf
              liftIO $ logGUI $ "Cairo.setSourceSurface buffer"
              Cairo.setSourceSurface surf  0.0  0.0
              liftIO (Gtk.regionGetRectangles region) >>= mapM_
                (\ (Gtk.Rectangle x y w h) -> do
                  let f = realToFrac :: Int -> Double
                  liftIO $ logGUI $ unwords
                    ["Cairo.rectangle", show x, show y, show w, show h, ">> Cairo.paint"]
                  Cairo.setOperator Cairo.OperatorSource
                  Cairo.rectangle (f x) (f y) (f w) (f h)
                  Cairo.paint
                )
#else
            logGUI $ "Gtk.gcSetClipRegion"
            Gtk.gcSetClipRegion (theGtkGraphCtx livest) region
            (w, h) <- Gtk.drawableGetSize (theGtkPixmap livest)
            logGUI $ unwords ["Gtk.drawRectangle 0 0", show w, show h]
            Gtk.drawRectangle canvas (theGtkGraphCtx livest) True 0 0 w h
#endif
          return True
        ------------------------------------------------------------------------------------
        return True

-- | The init event handler was responsible for pulling the init GUI function from the
-- 'GtkWindowState' and evaluating it, then installing a configure event handler. 
installInitEventHandler :: GtkState ()
installInitEventHandler = do
  logGUI <- mkLogger "permanentHandlers" True
  env <- get
  liftIO $ do
    ------------------------------------ Initializing Event --------------------------------
    logGUI "Glib.on Gtk.configureEvent -- for init handler"
    initHandler <- newEmptyMVar
    (>>= (putMVar initHandler)) $ Glib.on (gtkWindow env) Gtk.configureEvent $ do
      logGUI <- mkLogger "initCallback" True
      canvas <- Gtk.eventWindow
      size   <- Gtk.eventSize
      let allocSize = dimsForAlloc size
      let evt = sampCoord <$> uncurry V2 size
      liftIO $ lockGtkWindow logGUI (GtkUnlockedWin $ thisWindow env) $ do
        live   <- gets gtkWindowLive
        liftIO $ do
          logGUI $ "newGtkWindowLive (dimsForAlloc " ++
            show size ++ " --> " ++ show allocSize ++ ")"
          livest <- newGtkWindowLive (currentConfig env) canvas allocSize
          logGUI "putMVar gtkWindowLive"
          putMVar live livest
          logGUI $ "initReaction " ++ show size
        use initReaction >>= flip evalConnectReact evt
        forceDisconnect initReaction
        liftIO $ do
          logGUI "Glib.signalDisconnect initCallback"
          takeMVar initHandler >>= Glib.signalDisconnect
          logGUI "installResizeEventHandler -- a permanent handler"
        installResizeEventHandler
      return False

-- | This function installs the configure event handler, which is called whenever the window is
-- resized.
installResizeEventHandler :: GtkState ()
installResizeEventHandler = do
  logGUI <- mkLogger "installResizeEventHandler" True
  env <- get
  liftIO $ do
    logGUI "Glib.on window Gtk.configureEvent"
    void $ Glib.on (gtkWindow env) Gtk.configureEvent $ do
      logGUI <- mkLogger "configureEventCallback" True
      canvas <- Gtk.eventWindow
      size   <- Gtk.eventSize
      let evt = sampCoord <$> uncurry V2 size
      liftIO $ lockGtkWindow logGUI (GtkUnlockedWin $ thisWindow env) $ do
        liftIO $ logGUI $ "resizeGtkDrawContext " ++ show size
        resizeGtkDrawContext canvas size
        liftIO $ logGUI "resizeReaction"
        use resizeReaction >>= flip evalConnectReact evt
        liftIO $ do
          logGUI "Gtk.widgetQueueDraw"
          Gtk.widgetQueueDraw (gtkWindow env)
          return False

-- | This event handler is responsible for cleaning up when a window close event occurs. Whether the
-- application quit or not when the window closes is configurable by parameters in the
-- 'Happlets.Config.Config' data structure.
installDeleteEventHandler :: GtkState ()
installDeleteEventHandler = do
  logGUI <- mkLogger "installDeleteEventHandler" True
  env <- get
  liftIO $ do
    logGUI "Glib.on Gtk.deleteEvent"
    Glib.on (gtkWindow env) Gtk.deleteEvent $ liftIO $
      lockGtkWindow logGUI (GtkUnlockedWin $ thisWindow env) $ do
        logGUI <- mkLogger "deleteWindowCallback" True
        let cfg = currentConfig env
        if cfg ^. deleteWindowOnClose
         then do
          liftIO $ logGUI "Gtk.widgetDestroy -- willDeleteWindowOnClose is True"
          disconnectAll
          liftIO $ Gtk.widgetDestroy (gtkWindow env)
         else liftIO $ do
          logGUI $ "Gtk.widgetHideAll -- willDeleteWindowOnClose is False"
          Gtk.widgetHideAll (gtkWindow env)
        liftIO $ if not (cfg ^. quitOnWindowClose)
         then logGUI "-- quitOnWindowClose is False"
         else do
          logGUI $ "Gtk.mainQuit -- quitOnWindowClose is True"
          Gtk.mainQuit
        return True
    logGUI "-- done installing permanent event handlers"

deleteWin :: GtkWindowState -> IO ()
deleteWin env = do
  logGUI <- mkLogger "deleteWin" True
  logGUI "Gtk.widgetDestroy"
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
  logGUI <- mkLogger "resizeGtkDrawContext" True
  env <- get
  let cfg = currentConfig env
  let newDims = dimsForAlloc size
  liftIO $ logModMVar_ logGUI "gtkWindowLive" (gtkWindowLive env) $ \ livest -> do
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
      logGUI $ "-- will not re-allocate window buffer, old buffer size = " ++
        show oldDims ++ ", new buffer size = " ++ show newDims
      return livest
     else do
      logGUI $ "gtkAllocNewPixmap " ++ show newDims
      newGtkWindowLive cfg canvas newDims

-- | Allocates a new 'GtkWindowLive', including the 'Gtk.Pixmap' buffer and 'Gtk.GC' graphics
-- context. The buffer allocated will be exactly the dimensions given without checking if the size
-- is valid, and invalid demensions will crash the thread.
newGtkWindowLive :: Config -> Gtk.DrawWindow -> (Int, Int) -> IO GtkWindowLive
newGtkWindowLive cfg canvas (w, h) = do
  logGUI <- mkLogger "gtkAllocNewPixmap" True
#if USE_CAIRO_SURFACE_BUFFER
  let depth = if isJust $ cfg ^. backgroundTransparency
        then Cairo.FormatARGB32
        else Cairo.FormatRGB24
  logGUI $ unwords ["Cairo.createImageSurface", show depth, show w, show h]
  surface <- Cairo.createImageSurface depth w h
#else
  let grey  = cfg ^. backgroundGreyValue
  let depth = Just $ if isJust $ cfg ^. backgroundTransparency then 32 else 24
  logGUI $ "Gtk.pixmapNew " ++ show (w, h) ++ ' ' : show depth
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

disconnectAll :: GtkState ()
disconnectAll = do
  forceDisconnect visibilityReaction
  forceDisconnect focusReaction
  forceDisconnect mouseHandler
  forceDisconnect cursorHandler
  forceDisconnect keyHandler
  forceDisconnect animatorThread
  forceDisconnect resizeReaction

----------------------------------------------------------------------------------------------------

-- | This is a type of 'Happlets.GUI.GUI' function where the @window@ type is 'GtkWindow' and the
-- @draw@ type is 'GtkDraw' (a Cairo 'Graphics.Rendering.Cairo.Render' function).
type GtkGUI model a = GUI GtkWindow model a

-- | Creates a happlet and associates it with a window.
gtkNewWindow :: Config -> IO GtkWindow
gtkNewWindow cfg = do
  logGUI <- mkLogger "gtkWindowNew" True
  logGUI "createWin"
  GtkUnlockedWin . thisWindow <$> createWin cfg

gtkWindowVisible :: Bool -> GtkGUI model ()
gtkWindowVisible visible = do
  logGUI <- mkLogger ("gtkWindowVisible "++show visible) True
  runGtkStateGUI $ get >>= \ env -> liftIO $ do
    if visible
     then do
      logGUI "Gtk.widgetShow"
      Gtk.widgetShowAll (gtkWindow env)
      logGUI "Gtk.widgetQueueDraw"
      Gtk.widgetQueueDraw (gtkWindow env)
     else do
      logGUI "Gtk.widgetHideAll"
      Gtk.widgetHideAll (gtkWindow env)

gtkAttachHapplet :: Bool -> GtkWindow -> Happlet model -> (PixSize -> GtkGUI model ()) -> IO ()
gtkAttachHapplet showWin win happ init = do
  logGUI <- mkLogger "gtkAttachHapplet" True
  logGUI "lockGtkWindow"
  lockGtkWindow logGUI win $ do
    liftIO $ logGUI "disconnectAll -- on current window"
    disconnectAll
    initReaction .= ConnectReact
      { doDisconnect = return () -- This function is disconnected automatically by the init callback.
      , doReact = \ size -> do
          logGUI <- mkLogger "initReaction" True
          env <- get
          liftIO $ logGUI $ "gtkRunHapplet " ++ show size
          void $ gtkRunHapplet happ (const $ return ()) $ init size
          when showWin $ liftIO $ do
            logGUI "Gtk.widgetShowAll"
            Gtk.widgetShowAll (gtkWindow env)
            logGUI "Gtk.widgetQueueDraw"
            Gtk.widgetQueueDraw (gtkWindow env)
          return False
      }
    win <- gets gtkWindow
    liftIO $ do
      logGUI "Gtk.widgetShow"
      Gtk.widgetShow win

-- | Change the happlet displayed in the current window. Remove the current Happlet event handlers
-- and re-install the event handlers for the given Happlet.
gtkSetHapplet :: Happlet newmodel -> (PixSize -> GtkGUI newmodel ()) -> GtkGUI oldmodel ()
gtkSetHapplet newHapp init = do
  logGUI <- mkLogger "windowSetHapplet" True
  oldHapp <- askHapplet
  if sameHapplet oldHapp newHapp
   then liftIO $ logGUI "-- the Happlet given is the one that is already attached to this window"
   else do
    liftIO $ logGUI "disconnectAll -- on current window"
    runGtkStateGUI disconnectAll
    st <- getGUIState
    (env, _) <- case theGUIWindow st of
      GtkUnlockedWin{} -> error $ "gtkSetHapplet: " ++
        "Evaluated a GtkState function within a GtkGUI function on a locked GtkWindow."
        -- The 'window' value passed to the 'evalGUI' function must be a 'GtkLockedWin' constructor.
      GtkLockedWin env -> liftIO $ do
        size <- Gtk.widgetGetDrawWindow (gtkWindow env) >>= Gtk.drawableGetSize
        let evt = sampCoord <$> uncurry V2 size
        logGUI $ "onHapplet -- run initializer " ++ show size
        onHapplet newHapp $
          liftM (theGUIWindow &&& theGUIModel) . evalGUI (init evt) newHapp (GtkLockedWin env)
        --logGUI "Gtk.widgetQueueDraw"
        --Gtk.widgetQueueDraw $ gtkWindow win
        --logGUI "Gtk.widgetShow"
        --Gtk.widgetShow $ gtkWindow win
    putGUIState $ st{ theGUIWindow = env  }

-- Set which function redraws the window.
evalCairoOnCanvas :: CairoRender a -> GtkState a
evalCairoOnCanvas redraw = do
  logGUI <- mkLogger "evalCairoOnCanvas" True
  env <- get
  (a, rendst) <- liftIO $ logWithMVar logGUI "gtkWindowLive" (gtkWindowLive env) $ \ livest -> do
    (w, h) <- Gtk.drawableGetSize (gtkDrawWindow livest)
#if USE_CAIRO_SURFACE_BUFFER
    let cairoRenderer = Cairo.renderWith (theCairoSurface livest)
    logGUI $ "Cairo.renderWith"
#else
    let cairoRenderer = Gtk.renderWithDrawable (theGtkPixmap livest)
    logGUI "Gtk.renderWithDrawable -- to theGtkPixmap"
#endif
    (a, rendst) <- cairoRenderer $
      runCairoRender (sampCoord <$> V2 w h) (env ^. cairoRenderState) redraw
    region <- Gtk.regionRectangle $ Gtk.Rectangle 0 0 w h
    logGUI "Gtk.drawWindowInvalidateRegion"
    Gtk.drawWindowInvalidateRegion (gtkDrawWindow livest) region True
    return (a, rendst)
  cairoRenderState .= rendst
  return a

-- Draw directly to the window.
evalCairoOnGtkDrawable :: CairoRender a -> GtkState a
evalCairoOnGtkDrawable redraw = do
  logGUI <- mkLogger "evalCairoOnGtkDrawable" True
  env <- get
  (a, rendst) <- liftIO $ logWithMVar logGUI "gtkWindowLive" (gtkWindowLive env) $ \ livest -> do
    (w, h) <- Gtk.drawableGetSize (gtkDrawWindow livest)
    logGUI "Gtk.renderWithDrawable -- to theGtkDrawWindow"
    (a, rendst) <- Gtk.renderWithDrawable (gtkDrawWindow livest) $
      runCairoRender (sampCoord <$> V2 w h) (env ^. cairoRenderState) redraw
    return (a, rendst)
  cairoRenderState .= rendst
  return a

----------------------------------------------------------------------------------------------------

instance HappletWindow GtkWindow CairoRender where
  getWindowSize       = runGtkStateGUI $ do
    logGUI <- mkLogger "getWindowSize" True
    env <- get
    liftIO $ logWithMVar logGUI "gtkWindowLive" (gtkWindowLive env) $ \ livest -> do
      -- Obtain the size from the draw window, but will not draw to the draw window.
      (w, h) <- Gtk.drawableGetSize (gtkDrawWindow livest)
      return $ V2 (sampCoord w) (sampCoord h)

  windowChangeHapplet = gtkSetHapplet
  onCanvas            = runGtkStateGUI . evalCairoOnCanvas
  onOSBuffer          = runGtkStateGUI . evalCairoOnGtkDrawable

  refreshRegion rects = runGtkStateGUI $ do
    logGUI <- mkLogger "refreshRegion" True
    env <- get
    liftIO $ logWithMVar logGUI "gtkWindowLive" (gtkWindowLive env) $ \ livest ->
      Gtk.renderWithDrawable (gtkDrawWindow livest) $
        forM_ (fmap realToFrac . canonicalRect2D <$> rects) $ \ rect -> do
          Cairo.setOperator Cairo.OperatorSource
          Cairo.setSourceSurface (theCairoSurface livest) (0.0) (0.0)
          let (x, y) = (rect) ^. rect2DHead . pointXY
          let (w, h) = ((rect ^. rect2DTail) - (rect ^. rect2DHead)) ^. pointXY
          Cairo.rectangle x y w h
          Cairo.fill

  refreshWindow      = runGtkStateGUI $ do
    logGUI <- mkLogger "refreshWindow" True
    env <- get
    liftIO $ logWithMVar logGUI "gtkWindowLive" (gtkWindowLive env) $ \ livest -> do
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
    let loc@(TextGridLocation (TextGridRow dbgRow) (TextGridColumn dbgCol)) = st ^. textCursor
    traceM $ "\nprinch "++show c++" @"++show dbgRow++","++show dbgCol
    loc0@(V2 x0 _y0) <- (+ (off - V2 (2.0) (3.0))) <$> gridTextLocationToPoint loc
    dLoc <- fmap realToFrac <$> getPixSizeOfChar (fs ^. fontSize) c
    traceM $ "startPoint="++show loc0++" rectSize="++show dLoc
    rendst <- CairoRender get
    --rendst <- case rendst ^. minFontExtents of
    --  Just{}  -> return rendst
    --  Nothing -> do
    --    extns <- cairoRender $ cairoGetMinFontExtents <* cairoSetFontStyle fs
    --    CairoRender $ do
    --      traceM $ "init minFontDesc "++show (Cairo.fontExtentsDescent extns)
    --      minFontExtents .= Just extns
    --      cairoScreenPrinterState . printerFontStyle .= fs
    --      get
    let loc1@(V2 _x1 y1) = loc0 + dLoc
    extns  <- cairoRender $ do
      Cairo.setOperator Cairo.OperatorSource
      traceM $ "bgRect "++show loc0++"+"++show dLoc
      cairoDrawRect bgcolor (0.0) bgcolor
        (rect2D & rect2DHead .~ (realToFrac <$> loc0) & rect2DTail .~ (realToFrac <$> loc1))
      cairoSetColorRGBA32 $ fs ^. fontForeColor
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
cairoSetColorRGBA32 :: Color -> Cairo.Render ()
cairoSetColorRGBA32 = unpackRGBA32Color >>> \ (r,g,b,a) -> Cairo.setSourceRGBA r g b a

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
  cairoSetColorRGBA32 color
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
        cairoSetColorRGBA32 color
        Cairo.setLineCap Cairo.LineCapRound
        Cairo.setLineWidth $ unwrapRealApprox width
        Cairo.setLineJoin Cairo.LineJoinMiter
        cairoMoveTo a
        forM_ ax cairoLineTo
        Cairo.stroke
  in  \ case { [] -> return (); [a] -> run a [a]; a:ax -> run a ax; }

cairoDrawRect :: LineColor -> LineWidth -> FillColor -> Rect2D RealApprox -> Cairo.Render ()
cairoDrawRect lineColor width fillColor rect = do
  cairoSetColorRGBA32 fillColor
  cairoMoveTo $ rect ^. rect2DHead
  let (head, tail) = (unwrapRealApprox <$> canonicalRect2D rect) ^. rect2DPoints
  let (x, y) = head ^. pointXY
  let (w, h) = (tail - head) ^. pointXY
  Cairo.rectangle x y w h
  Cairo.fill
  when (width > 0.0) $ do
    cairoSetColorRGBA32 lineColor
    Cairo.setLineWidth $ unwrapRealApprox width
    Cairo.setLineJoin Cairo.LineJoinMiter
    Cairo.rectangle x y w h
    Cairo.stroke

-- | This is a helpful function you can use for your 'Happlet.Control.controlRedraw' function to clear
-- the window with a background color, given by the four 'Prelude.Double' parameters for Red, Green,
-- Blue, and Alpha (in that order).
cairoClearCanvas :: Double -> Double -> Double -> Double -> Cairo.Render ()
cairoClearCanvas r g b a = do
  logGUI <- mkLogger "cairoClearCanvas" True
  liftIO $ logGUI $ unwords $ show <$> [r,g,b,a]
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
  visibleEvents = installEventHandler "visibleEvents" visibilityReaction $ \ logGUI env next -> do
    let win = gtkWindow env
    logGUI "Glib.on Gtk.visibilityNotifyEvent"
    vis <- Glib.on win Gtk.visibilityNotifyEvent $
      Gtk.eventVisibilityState >>=
      liftIO . next . not . (== Gtk.VisibilityFullyObscured) >>
      return False
    return $ do
      logGUI "Glib.signalDisconnect Gtk.visibilityNotifyEvent"
      Glib.signalDisconnect vis
  focusEvents   = installEventHandler "focusEvents" focusReaction $ \ logGUI env next -> do
    let win = gtkWindow env
    logGUI "Glib.on Glib.focusInEvent"
    focin  <- Glib.on win Gtk.focusInEvent  $ liftIO (next True)  >> return False
    logGUI "Glib.on Glib.focusOutEvent"
    focout <- Glib.on win Gtk.focusOutEvent $ liftIO (next False) >> return False
    return $ do
      logGUI "Glib.signalDisconnect (Glib.on Glib.focusInEvent)"
      Glib.signalDisconnect focin
      logGUI "Glib.signalDisconnect (Glib.on Glib.focusOutEvent)"
      Glib.signalDisconnect focout

instance CanResize GtkWindow where
  resizeEvents = installEventHandler "resizeEvents" resizeReaction {-[]-} (\ _ _ _ -> return $ pure ())

data AnimationThreadControl
  = AnimationThreadControl
    { animationThreadAlive :: !Bool
    , animationInitTime    :: !UTCTime
    }

instance CanAnimate GtkWindow where
  animationIsRunning = runGtkStateGUI $
    gets theAnimatorThread <&> \ case { Disconnected -> False; _ -> True; }
  stepFrameEvents react = do
    flip (installEventHandler "stepFrameEvents" animatorThread {-[]-}) react $ \ logGUI env next -> do
      let rate = env & theAnimationFrameRate . currentConfig
      t0    <- getCurrentTime
      t0ref <- newIORef AnimationThreadControl
        { animationThreadAlive = True
        , animationInitTime    = t0
        }
      logGUI "Glib.on Glib.timeoutAdd"
      frame <- flip Glib.timeoutAdd (min 200 $ floor (1000.0 / rate)) $ do
        AnimationThreadControl{animationThreadAlive=alive,animationInitTime=t0} <- readIORef t0ref
        if not alive then return False else 
          diffUTCTime <$> getCurrentTime <*> pure t0 >>= next . realToFrac
      --
      -- TODO: the 'next' function needs to be evaluated here at time 0, becuase 'timeoutAdd' does
      -- not call it until after the first time step interval has passed. For slow animations, this
      -- will cause a noticable delay between the time the 'stepFrameEvents' function is evaluated
      -- and the first frame callback is evaluated.
      return $ do
        logGUI "Glib.signalDisconnect (Glib.on Glib.timeoutAdd)"
        modifyIORef t0ref $ \ ctrl -> ctrl{ animationThreadAlive = False }
        Glib.timeoutRemove frame

instance CanKeyboard GtkWindow where
  keyboardEvents = installEventHandler "keyboardEvents" keyHandler $ \ logGUI env next -> do
    let box = gtkEventBox env
    logGUI "Glib.on Gtk.keyReleaseEvent"
    press   <- Glib.on box Gtk.keyReleaseEvent $
      handleKey False >>= liftIO . next >> return False
    logGUI "Glib.on Gtk.keyPressEvent"
    release <- Glib.on box Gtk.keyPressEvent $
      handleKey True  >>= liftIO . next >> return False
    return $ do
      logGUI "Glib.signalDisconnect Gtk.keyReleaseEvent"
      Glib.signalDisconnect press
      Glib.signalDisconnect release

instance CanMouse GtkWindow where
  providedMouseDevices = return []
  mouseEvents = \ case
    MouseButton -> installEventHandler "mouseButtonEvents" mouseHandler $ \ logGUI env next -> do
      let box = gtkEventBox env
      logGUI "Glib.on Gtk.buttonPressEvent"
      press <- Glib.on box Gtk.buttonPressEvent $
        handleMouse True >>= liftIO . next >> return False
      release <- Glib.on box Gtk.buttonReleaseEvent $
        handleMouse False >>= liftIO . next >> return False
      return $ do
        logGUI "Glib.signalDisconnect Gtk.buttonPressEvent"
        Glib.signalDisconnect press
        Glib.signalDisconnect release
    MouseDrag   -> installEventHandler "mouseDragEvents" mouseHandler $ \ logGUI env next -> do
      let box = gtkEventBox env
      logGUI "Glib.on Gtk.buttonPressEvent"
      press <- Glib.on box Gtk.buttonPressEvent $
        handleMouse True >>= liftIO . next >> return False
      logGUI "Glib.on Gtk.buttonReleaseEvent"
      release <- Glib.on box Gtk.buttonReleaseEvent $
        handleMouse False >>= liftIO . next >> return False
      logGUI "Glib.on Gtk.motionNotifyEvent"
      button <- Glib.on box Gtk.motionNotifyEvent $
        handleCursor True >>= liftIO . next >> return False
      logGUI "Gtk.widgetAddEvents [Gtk.ButtonMotionMask]"
      Gtk.widgetAddEvents box [Gtk.ButtonMotionMask]
      return $ do
        -- NOTE that you cannot call 'Gtk.widgetDelEvents' unless the widget is not realized. Doing
        -- so results in an assertion error being thrown in the log file. I assume the previous
        -- widget mask is restored once the signals are disconnected, but I don't know.
        logGUI "Glib.signalDisconnect Gtk.buttonPressEvent"
        Glib.signalDisconnect press
        Glib.signalDisconnect release
        Glib.signalDisconnect button
    MouseAll    -> installEventHandler "mouseAllEvents" mouseHandler $ \ logGUI env next -> do
      let box = gtkEventBox env
      logGUI "Glib.on Gtk.motionNotifyEvent"
      button  <- newIORef False
      press   <- Glib.on box Gtk.buttonPressEvent   $
        liftIO (writeIORef button False) >>
        handleMouse True  >>= liftIO . next >> return False
      release <- Glib.on box Gtk.buttonReleaseEvent $
        liftIO (writeIORef button True) >>
        handleMouse False >>= liftIO . next >> return False
      motion  <- Glib.on box Gtk.motionNotifyEvent  $
        liftIO (readIORef button) >>=
        handleCursor >>= liftIO . next >> return False
      logGUI "Gtk.widgetAddEvents [Gtk.PointerMotionMask, Gtk.ButtonMotionMask]"
      Gtk.widgetAddEvents box [Gtk.PointerMotionMask, Gtk.ButtonMotionMask]
      return $ do
        -- NOTE that you cannot call 'Gtk.widgetDelEvents' unless the widget is not realized. Doing
        -- so results in an assertion error being thrown in the log file. I assume the previous
        -- widget mask is restored once the signals are disconnected, but I don't know.
        logGUI "Glib.signalDisconnect Gtk.motionNotifyEvent"
        Glib.signalDisconnect press
        Glib.signalDisconnect release
        Glib.signalDisconnect motion

-- | This function enables an event handler, and takes a continuation which will be evaluated by
-- this function call to do the low-level work of install the event handler into the Gtk
-- window. This function also does the work of taking a 'Happlets.GUI.GUI' function to be evaluated
-- on each event, converting this function to a 'ConnectReact' function, and installing it into the
-- 'GtkWindow's internal state so that it can be actually evaluated every time the low-level event
-- handler is evaluated. The result is a function that can be used to instantiate any of the
-- "Happlets.GUI" module's event handler classes.
installEventHandler
  :: String
  -> Lens' GtkWindowState (ConnectReact event)
  -> (LogGUI -> GtkWindowState -> (event -> IO Bool) -> IO (IO ()))
  -> (event -> GtkGUI model ())
  -> GtkGUI model ()
installEventHandler logString connectReact install react = do
  logGUI <- mkLogger logString True
  happ   <- askHapplet
  liftIO $ logGUI "runGtkStateGUI"
  runGtkStateGUI $ do
    env <- get
    callback <- use connectReact
    disconnect <- case callback of
      ConnectReact{doDisconnect=disconnect} -> do
        liftIO $ logGUI "-- event handler already installed"
        return disconnect
      Disconnected -> liftIO $ do
        logGUI "-- install event handler"
        disconnect <- install logGUI env $ \ event -> do
          logGUI "lockGtkWindow"
          lockGtkWindow logGUI (GtkUnlockedWin $ thisWindow env) $ do
            stillAlive <- use connectReact >>= flip evalConnectReact event
            unless stillAlive $ forceDisconnect connectReact
            return stillAlive
        return $ liftIO disconnect
    liftIO $ logGUI "resizeReaction -- now enabled"
    connectReact .= ConnectReact
      { doReact = gtkRunHapplet happ (checkGUIContinue logGUI resizeReaction) . react
      , doDisconnect = disconnect
      }

handleMouse :: Pressed -> Gtk.EventM Gtk.EButton Mouse
handleMouse upDown = do
  logGUI <- mkLogger "handleMouse" True
  (x, y) <- Gtk.eventCoordinates
  button <- getMouseButton
  mods   <- packGtkModifiers <$> Gtk.eventModifierAll
  let evt = Mouse "" upDown mods button (V2 (round x) (round y))
  liftIO $ logGUI $ "handleMouse " ++ show evt
  return evt

handleCursor :: Pressed -> Gtk.EventM Gtk.EMotion Mouse
handleCursor upDown = do
  logGUI <- mkLogger "handleCursor" True
  (x, y) <- Gtk.eventCoordinates
  mods   <- packGtkModifiers <$> Gtk.eventModifierAll
  let evt = Mouse "" upDown mods MotionOnly (V2 (round x) (round y))
  liftIO $ logGUI $ "handleCursor " ++ show evt
  return evt

handleKey :: Pressed -> Gtk.EventM Gtk.EKey Keyboard
handleKey upDown = do
  logGUI <- mkLogger "handleKey" True
  key  <- Gtk.eventKeyVal
  mods <- packGtkModifiers <$> Gtk.eventModifierAll
  let evt = RawKey upDown mods key
  liftIO $ logGUI $ "handleKey " ++ show evt
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
      Gtk.Shift   -> [Shift]
      Gtk.Lock    -> [CapsLock]
      Gtk.Control -> [Ctrl]
      Gtk.Alt     -> [Alt1]
      Gtk.Alt2    -> [Alt2]
      Gtk.Super   -> [Super1]
      Gtk.Hyper   -> [Super2]
      _           -> []
  )

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
  logGUI <- mkLogger "gdkBlit" True
  --region <- screenMasksToGdkRegion masks (fromIntegral dx, fromIntegral dy)
  (w, h) <- Gtk.drawableGetSize canvas
  -- TODO: This creates an update region for the whole window. This needs to be made more efficient,
  -- such that only the minimal updated region is redrawn.
  region <- Gtk.regionNew
  Gtk.regionRectangle (Gtk.Rectangle 0 0 w h) >>= Gtk.regionUnion region
  logGUI $ "Gtk.gcNewWithValues " ++ show dx ++ ' ' : show dy
  gc <- Gtk.gcNewWithValues canvas $ Gtk.newGCValues
    { Gtk.tile = Just pixmap
    , Gtk.fill = Gtk.Tiled
    , Gtk.tsXOrigin = fromIntegral dx
    , Gtk.tsYOrigin = fromIntegral dy
    }
  Gtk.gcSetClipRegion gc region
  logGUI $ "Gtk.drawRectangle 0 0 " ++ show w ++ ' ' : show h
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
  newImageBuffer size@(V2 w h) draw = runGtkStateGUI $ do
    rendst <- use cairoRenderState
    (a, rendst, img) <- liftIO $ do
      logGUI <- mkLogger "newImageBuffer" True
      logGUI $ unwords ["Cairo.createImageSurface Cairo.FormatARGB32", show w, show h]
      surface <- Cairo.createImageSurface Cairo.FormatARGB32 (fromIntegral w) (fromIntegral h)
      logGUI $ "Cairo.renderWith draw"
      (a, rendst) <- Cairo.renderWith surface $ runCairoRender size rendst draw
      mvar <- newMVar surface
      return (a, rendst, GtkImage{gtkCairoSurfaceMVar=mvar})
    cairoRenderState .= rendst
    return (a, img)

  resizeImageBuffer (GtkImage{gtkCairoSurfaceMVar=mvar}) size@(V2 w h) draw = runGtkStateGUI $ do
    rendst <- use cairoRenderState
    (a, rendst) <- liftIO $ do
      logGUI <- mkLogger "resizeImageBuffer" True
      logModMVar logGUI "gtkCairoSurfaceMVar" mvar $ \ surface -> do
        oldDims <- (,)
          <$> Cairo.imageSurfaceGetWidth  surface
          <*> Cairo.imageSurfaceGetHeight surface
        let f (SampCoord a) = fromIntegral a
        surface <- if oldDims == (f w, f h) then return surface else do
          logGUI $ unwords ["Cairo.createImageSurface Cairo.FormatARGB32", show w, show h]
          Cairo.createImageSurface Cairo.FormatARGB32 (fromIntegral w) (fromIntegral h)
        logGUI $ "Cairo.renderWith draw"
        (a, rendst) <- Cairo.renderWith surface $ runCairoRender size rendst draw
        return (surface, (a, rendst))
    cairoRenderState .= rendst
    return a

  drawImage (GtkImage{gtkCairoSurfaceMVar=mvar}) draw = runGtkStateGUI $ do
    rendst <- use cairoRenderState
    (a, rendst) <- liftIO $ do
      logGUI <- mkLogger "drawImage" True
      logWithMVar logGUI "gtkCairoSurfaceMVar" mvar $ \ surface -> do
        logGUI "Cairo.renderWith draw"
        size <- V2
          <$> Cairo.imageSurfaceGetWidth  surface
          <*> Cairo.imageSurfaceGetHeight surface
        (a, rendst) <- Cairo.renderWith surface $ runCairoRender (sampCoord <$> size) rendst draw
        return (a, rendst)
    cairoRenderState .= rendst
    return a

  blitImage (GtkImage{gtkCairoSurfaceMVar=mvar}) offset = runGtkStateGUI $ do
    logGUI <- mkLogger "blitImage" True
    liveMVar <- gets gtkWindowLive
    liftIO $ logWithMVar logGUI "gtkWindowLive" liveMVar $ \ liveEnv -> do
      logGUI "modifyMVar gtkCairoSurfaceMVar -- begin"
      logWithMVar logGUI "gtkCairoSurfaceMVar" mvar $ \ surface -> do
        Cairo.renderWith (theCairoSurface liveEnv) $ do
          let (V2 x y) = realToFrac <$> offset
          liftIO $ logGUI $ unwords ["Cairo.setSourceSurface", show x, show y, ">> Cairo.paint"]
          Cairo.setOperator Cairo.OperatorSource
          Cairo.setSourceSurface surface x y
          Cairo.paint

  blitImageTo (GtkImage{gtkCairoSurfaceMVar=src}) (GtkImage{gtkCairoSurfaceMVar=targ}) offset =
    runGtkStateGUI $ liftIO $ if src == targ then error "cannot blit image to itself" else do
      logGUI <- mkLogger "blitImageTo" True
      logWithMVar logGUI "blitSource" src $ \ src ->
        logWithMVar logGUI "blitTarget" targ $ \ targ ->
          Cairo.renderWith targ $ do
            let (V2 x y) = realToFrac <$> offset
            liftIO $ logGUI $ unwords ["Cairo.setSourceSurface", show x, show y, ">> Cairo.paint"]
            Cairo.setOperator Cairo.OperatorSource
            Cairo.setSourceSurface src x y
            Cairo.paint

#else
  newImageBuffer  (V2 (SampCoord w) (SampCoord h)) draw = liftIO $ do
    pixmap <- Gtk.pixmapNew (Nothing :: Maybe Gtk.Pixmap)
      (fromIntegral w) (fromIntegral h) (Just 32)
    Gtk.renderWithDrawable pixmap $ runCairoRender draw
    mvar <- newMVar pixmap
    return GtkImage
      { gtkPixmapMVar = mvar
      }

  resizeImageBuffer (GtkImage{gtkPixmapMVar=mvar}) (V2 w h) draw = do
    logGUI <- mkLogger "resizeImageBuffer" True
    liftIO $ logModMVar logGUI mvar $ \ pixmap -> do
      pixmap <- Gtk.pixmapNew (Just pixmap) (fromIntegral w) (fromIntegral h) (Just 32)
      a <- Gtk.renderWithDrawable pixmap $ runCairoRender draw
      return (pixmap, a)

  drawImage (GtkImage{gtkPixmapMVar=mvar}) draw = do
    logGUI <- mkLogger "drawImage" True
    liftIO $ logWithMVar logGUI mvar $ flip Gtk.renderWithDrawable $ runCairoRender draw

  blitImage (GtkImage{gtkPixmapMVar=mvar}) offset = runGtkStateGUI $ do
    logGUI <- mkLogger "blitImage" True
    gtkwin <- gets gtkWindow
    canvas <- liftIO $ Gtk.widgetGetDrawWindow gtkwin
    liftIO $ logWithMVar logGUI mvar $ \ pixmap -> gdkBlit canvas pixmap offset

  blitImageTo (GtkImage{gtkPixmapMVar=src}) (GtkImage{gtkPixmapMVar=targ}) offset = do
    logGUI <- mkLogger "blitImageTo" True
    liftIO $ logWithMVar logGUI src $ \ src -> logWithMVar logGUI targ $ \ targ ->
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
      , theAnimationFrameRate      = 60.0
      , willDecorateWindow         = True
      , willQuitOnWindowClose      = False
      , willDeleteWindowOnClose    = False
      }
  , doInitializeGUI         = gtkInit
  , doGUIEventLoopLaunch    = gtkLaunchEventLoop
  , doWindowNew             = gtkNewWindow
  , doWindowDelete          = \ env -> do
      logGUI <- mkLogger "doWindowDelete" True
      lockGtkWindow logGUI env $ get >>= liftIO . deleteWin
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
    logGUI <- mkLogger "gtkInit" True
    logGUI "Gtk.initGUI"
    Gtk.initGUI >>= mapM_ logGUI
    logModMVar_ logGUI "gtkInitCheck" gtkInitCheck $ return . const True

-- | Launch the Gtk+ main event loop. This function is usually called via the
-- 'Happlets.Initialize.launchGUIEventLoop' function, which is automatically called by the
-- 'Happlets.Initialize.happlet' function.
gtkLaunchEventLoop :: Config -> IO ()
gtkLaunchEventLoop cfg = do
  logGUI <- mkLogger "gtkEventLoop" True
  let appName = cfg ^. registeredAppName
  unless (Strict.null appName) $ do
    logGUI $ "Glib.setProgramName " ++ show appName
    Glib.setProgramName appName
    logGUI $ "Glib.setApplicationName " ++ show appName
    Glib.setApplicationName appName
  logGUI "Gtk.mainGUI"
  Gtk.mainGUI
