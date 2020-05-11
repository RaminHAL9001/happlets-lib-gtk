{-# LANGUAGE CPP #-}

-- | This is the Gtk+ version 2 back-end for Happlets, and will also serve as the Happlet referecne
-- implementation. Creating a window automatically calls the the Gtk+ initializer. Use
-- 'newGtkWindow' to create a new window, and any of the "Happlet.World" functinos to manipulate the
-- windows. This module re-exports the "Happlets" module so it is not necessary to import both.
module Happlets.Provider.Gtk2
  ( gtkHapplet, gtkAnimationFrameRate, GtkGUI,
    GtkWindow, gtkLaunchEventLoop,
    CairoPixelBuffer,
    -- * Cairo Wrapper Functions
    -- | Functions that allow you to call directly into a "Graphics.Rendering.Cairo".'Cairo.Render'
    -- function, but using point, line, and color values specified in the "Happlets.Draw"
    -- sub-modules.
    CairoRender, cairoRender,
    --GtkCairoDiagram, gtkCairoDiagram,
    cairoClearCanvas, cairoSetColor, cairoGridLocationOfPoint,
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
import           Happlets.Model.Audio
import           Happlets.Model.GUI
import           Happlets.View
import           Happlets.Provider
import           Happlets.Provider.Cairo      hiding (mkLogger)
import           Happlets.Provider.ALSA       hiding (mkLogger)
import           Happlets.Provider.Gtk2.Debug

import           Control.Arrow
import           Control.Concurrent
import           Control.Exception

import           Data.Array.MArray
import           Data.Bits
import qualified Data.Vector.Unboxed as UVec
import           Data.IORef
import qualified Data.Map            as Map
import           Data.Maybe
import qualified Data.Text           as Strict
import           Data.Time.Clock
import           Data.Word

import qualified Graphics.Rendering.Cairo           as Cairo
import qualified Graphics.Rendering.Cairo.Matrix    as Cairo

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

mkLogger
  :: (Monad m, MonadIO m)
  => String -> m (Log IO)
mkLogger func = return $ \ sel msg ->
  if sel .&. Happlets.Provider.Gtk2.debugThisModule == mempty then return () else do
  tid <- myThreadId
  traceM $ '[':show tid++"][Happlets.Lib.Gtk."++func++']':msg
{-# INLINE mkLogger #-}

logSubGtk :: Log IO -> DebugTag -> String -> GtkState a -> GtkState a
logSubGtk logIO sel msg f = get >>=
  liftIO . logSubIO logIO sel msg . runGtkState f >>= state . const
{-# INLINE logSubGtk #-}

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
    { currentConfig           :: !Config
    , thisWindow              :: !(MVar GtkWindowState)
      -- ^ While a GUI function is evaluating, 'theGUIWindow' is always locked, i.e. set to the
      -- 'GtkLockedWin' state. This is problematic for spawning new threads, because the new thread
      -- will need to share the lock with the thread that spawned it. To solve this problem, a
      -- reference to the lock is stored in the 'thisWindow' pointer. Whenever a new thread is
      -- created, usually when installing an event handler or when launching a 'Worker' thread, the
      -- 'GUIState' used to evaluate the 'GUI' function in the new thread is reset to contain the
      -- 'thisWindow' reference.
    , gtkWindowLive           :: !(MVar GtkWindowLive)
    , gtkWindow               :: !Gtk.Window
    , theGtkEventBox          :: !Gtk.EventBox
    , theCairoRenderState     :: !CairoRenderState
    , theAudioPlaybackThread  :: !AudioPlaybackThread
    , theGovernment           :: !WorkerUnion
    , theInitReaction         :: !(ConnectReact PixSize)
    , theResizeReaction       :: !(ConnectReact (OldPixSize, NewPixSize))
    , theVisibilityReaction   :: !(ConnectReact Bool)
    , theFocusReaction        :: !(ConnectReact Bool)
    , theMouseHandler         :: !(ConnectReact Mouse)
    , theKeyHandler           :: !(ConnectReact Keyboard)
    , theDetatchHandler       :: !(ConnectReact ())
    , theAnimatorThread       :: !(ConnectReact AnimationMoment)
    , theContextSwitcher      :: !(GtkState (EventHandlerControl ()))
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

audioPlaybackThread :: Lens' GtkWindowState AudioPlaybackThread
audioPlaybackThread = lens theAudioPlaybackThread $ \ a b -> a{ theAudioPlaybackThread = b }

gtkCurrentWindowSize :: Lens' GtkWindowState PixSize
gtkCurrentWindowSize = cairoRenderState . cairoKeepWinSize

initReaction :: Lens' GtkWindowState (ConnectReact PixSize)
initReaction = lens theInitReaction $ \ a b -> a{ theInitReaction = b }

visibilityReaction :: Lens' GtkWindowState (ConnectReact Bool)
visibilityReaction = lens theVisibilityReaction $ \ a b -> a{ theVisibilityReaction = b }

focusReaction :: Lens' GtkWindowState (ConnectReact Bool)
focusReaction = lens theFocusReaction $ \ a b -> a{ theFocusReaction = b }

resizeReaction :: Lens' GtkWindowState (ConnectReact (OldPixSize, NewPixSize))
resizeReaction = lens theResizeReaction $ \ a b -> a{ theResizeReaction = b }

mouseHandler :: Lens' GtkWindowState (ConnectReact Mouse)
mouseHandler = lens theMouseHandler $ \ a b -> a{ theMouseHandler = b }

keyHandler :: Lens' GtkWindowState (ConnectReact Keyboard)
keyHandler = lens theKeyHandler $ \ a b -> a{ theKeyHandler = b }

detatchHandler :: Lens' GtkWindowState (ConnectReact ())
detatchHandler = lens theDetatchHandler $ \ a b -> a{ theDetatchHandler = b }

-- When a context switch is performed, this value is set with a continuation to be called after the
-- 'windowChangeHapplet' function completes. After evaluation of the 'GtkGUI' function completes,
-- the state is evaluated -- every single event handler will evaluate this function. To ensure that
-- nothing happens unless it is set, this lens is used to set the callback to @return ()@ (a no-op)
-- prior to evaluation of the 'GtkGUI' procedure in the 'liftGUIintoGtkState' function.
contextSwitcher :: Lens' GtkWindowState (GtkState (EventHandlerControl ()))
contextSwitcher = lens theContextSwitcher $ \ a b -> a{ theContextSwitcher = b }

animatorThread :: Lens' GtkWindowState (ConnectReact AnimationMoment)
animatorThread = lens theAnimatorThread $ \ a b -> a{ theAnimatorThread = b }

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
  GtkUnlockedWin mvar -> logModMVar logIO (_locks<>sel) "GtkUnlockedWin" mvar $
    liftM (\ (a, b) -> (b, a)) . runGtkState f
  GtkLockedWin{}      -> fail
    "lockGtkWindow: evaluated on an already locked 'GtkLockedWin' window."

-- | This function evaluates 'Happlets.GUI.GUI' functions within event handlers. It evaluates to a
-- 'GtkState' function, which means you are required to have first evaluated 'lockGtkWindow'. This
-- function will then lock the 'Happlets.GUI.Happlet' and then evaluate the 'Happlets.GUI.GUI'
-- function. If the 'Happlets.GUI.GUI' function evaluates to 'Happlets.GUI.disable' or
-- 'Happlets.GUI.failGUI', then this function returns 'Prelude.False'. Otherwise, 'Prelude.True' is
-- returned.
liftGUIintoGtkState :: Happlet model -> GtkGUI model a -> GtkState (EventHandlerControl a)
liftGUIintoGtkState happlet f = do
  logIO <- mkLogger "liftGUIintoGtkState"
  -- (1) Always set 'contextSwitcher' to a no-op, it may be set to something else by @f@.
  Happlets.Provider.Gtk2.contextSwitcher .= return (EventHandlerContinue ())
  winst <- get
  (result, winst) <- liftIO $
    logSubIO logIO _locks "Lock Happlet, evaluate GUI function." $ 
      fmap fst $ onHapplet happlet $ \ model -> do
        (guiContin, guist) <- logSubIO logIO _infra "call runGUI" $ runGUI f GUIState
          { theGUIHapplet  = happlet
          , theGUIProvider = GtkLockedWin winst
          , theGUIModel    = model
          , theGUIWorkers  = theGovernment winst
          }
        case theGUIWindow guist of
          GtkLockedWin winst -> return ((guiContin, winst), theGUIModel guist)
          GtkUnlockedWin{}   -> error
            "'runGUI' successfully completed, but returned state containing a locked window."
  --
  put winst -- Save the GtkState state produced by evaluation of the GtkGUI function.
  --
  -- (2) Check if the result is 'HappletEventCancel', if it is, there is a chance that a context
  -- switch occurred. The context switcher is always evaluated on a cancel signal. If the context
  -- switcher was not changed by @f@ between (1) and (2), the 'contextSwitcher' field has therefore
  -- not been changed from the (return ()) value (a no-op) set at (1) and so no context switch
  -- occurs.
  case result of
    EventHandlerCancel -> winst ^. Happlets.Provider.Gtk2.contextSwitcher >>= \ case
      EventHandlerFail msg -> do
        -- TODO: here, display error dialog box and halt program.
        fail $ Strict.unpack msg
      _                    -> return ()
    _                  -> return ()
  return result

-- | This function is intended to be passed as a parameter to 'liftGUIintoGtkState' by event handling
-- functions which check if the 'Happlets.GUI.GUI' function evaluated to 'Happlets.GUI.disable'.
checkGUIContinue
  :: Log IO
  -> DebugTag
  -> String
  -> Lens' GtkWindowState (ConnectReact event)
  -> EventHandlerControl a
  -> GtkState ()
checkGUIContinue logIO sel what connectReact = \ case
  EventHandlerHalt       ->
    forceDisconnect logIO (_infra<>sel) (what++"(on EventHandlerHalt)") connectReact
  EventHandlerFail   msg ->
    forceDisconnect logIO (_infra<>sel) (what++"(on EventHandlerFail "++show msg++")") connectReact
  EventHandlerCancel     -> return ()
  EventHandlerContinue{} -> liftIO $ logIO (_infra<>sel) $ what++"-- evaluated to 'GUIControl'"

-- | This function should be evaluated from within a 'GtkGUI' function when it is necessary to
-- update the 'GtkWindowState', usually this is for installing or removing event handlers.
liftGtkStateIntoGUI :: DebugTag -> String -> GtkState a -> GtkGUI model a
liftGtkStateIntoGUI sel what f = do
  logIO <- mkLogger "liftGtkStateIntoGUI"
  getGUIState >>= \ gui -> case theGUIWindow gui of
    GtkUnlockedWin{} -> error $ "liftGtkStateIntoGUI: " ++
      "Evaluated a GtkState function within a GtkGUI function on a locked GtkWindow."
      -- The 'window' value passed to the 'evalGUI' function must be a 'GtkLockedWin' constructor.
    GtkLockedWin env -> do
      (a, env) <- liftIO $ logSubIO logIO (_infra<>sel) ("runGtkState "++what) $
        runGtkState f env
      putGUIState $ gui{ theGUIProvider = GtkLockedWin env }
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
  this  <- newEmptyMVar
  live  <- newEmptyMVar
  gov   <- newWorkerUnion
  audio <- newMVar (const $ return (0, 0))
  let env = GtkWindowState
        { currentConfig            = cfg
        , thisWindow               = this
        , gtkWindowLive            = live
        , gtkWindow                = window
        , theGtkEventBox           = eventBox
        , theCairoRenderState      = CairoRenderState
            { theCairoKeepWinSize        = V2 0 0
            , theCanvasResizeMode        = CanvasResizeClear
            , theCairoRenderMode         = VectorMode
            , theCanvasFillColor         = PaintSolidColor white
            , theCanvasStrokeColor       = PaintSolidColor black
            , theCairoGeometry           = CairoGeometry
                { theCairoShape = Draw2DReset
                , theCairoLineWidth = 0
                , theCairoBlitTransform = identity
                }
            , theCairoClipRect = rect2D
            , theCairoScreenPrinterState = screenPrinterState
            }
        , theAudioPlaybackThread   = StereoPlaybackThread Nothing audio
        , theGovernment            = gov
        , theInitReaction          = Disconnected
        , theResizeReaction        = Disconnected
        , theVisibilityReaction    = Disconnected
        , theFocusReaction         = Disconnected
        , theMouseHandler          = Disconnected
        , theKeyHandler            = Disconnected
        , theDetatchHandler        = Disconnected
        , theAnimatorThread        = Disconnected
        , theContextSwitcher       = return (EventHandlerContinue ())
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
      let evt = (env ^. gtkCurrentWindowSize, sampCoord <$> uncurry V2 size)
      liftIO $ lockGtkWindow logIO _winevt (GtkUnlockedWin $ thisWindow env) $ do
        logSubGtk logIO _winevt ("resizeGtkDrawContext "++show size) $
          resizeGtkDrawContext canvas size
        evalConnectReact logIO _winevt "resizeReaction" resizeReaction evt
        gtkCurrentWindowSize .= snd evt
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
  forceDisconnect logIO _ctxevt  "detatchHandler"     detatchHandler
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
  liftGtkStateIntoGUI _winevt ("gtkWindowVisible "++show visible) $ do
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

-- | This is the function behind the Happlet public API 'Happlets.Initialize.attachWindow'.
gtkAttachHapplet :: Bool -> GtkWindow -> Happlet model -> (PixSize -> GtkGUI model ()) -> IO ()
gtkAttachHapplet showWin win happ init = do
  logIO <- mkLogger "gtkAttachHapplet"
  lockGtkWindow logIO _ctxevt win $ do
    disconnectAll logIO
    initReaction .= ConnectReact
      { doDisconnect = return () -- no need, is disconnected automatically by the init callback.
      , doReact = \ size -> do
          logIO <- mkLogger "initReaction"
          eventControl <- logSubGtk logIO _ctxevt ("liftGUIintoGtkState "++show size) $
            liftGUIintoGtkState happ $ init size
          case eventControl of
            EventHandlerContinue () -> do
              env <- get
              when showWin $ liftIO $ do
                logIO _drawevt "Gtk.widgetShowAll"
                Gtk.widgetShowAll (gtkWindow env)
                logIO _drawevt "Gtk.widgetQueueDraw"
                Gtk.widgetQueueDraw (gtkWindow env)
            EventHandlerHalt        -> liftIO $ hPutStrLn stderr $
              "'deleteEventHandler' was called during Happlet initialization.\n" ++
              "Happlet will not attach to window."
            EventHandlerCancel      -> liftIO $ hPutStrLn stderr $
              "'cancelNow' was called during Happlet initialization.\n"++
              "Happlet will not attach to window."
            EventHandlerFail   msg  -> liftIO $ hPutStrLn stderr $
              "Happlet failed to initialize: "++show msg
              -- TODO: also show a modal dialog box reporting the error.
      }
    win <- gets gtkWindow
    liftIO $ do
      logIO _winevt "Gtk.widgetShow"
      Gtk.widgetShow win

-- | Change the happlet displayed in the current window, that is to say, this function performs a
-- context switch. It removes the current Happlet event handlers and re-installs the event handlers
-- for the given Happlet using the given initializer. Note that this function never returns, as the
-- current happlet which evaluated this function will be halted so the context can switch over to
-- the new Happlet.
gtkSetHapplet :: Happlet newmodel -> (PixSize -> GtkGUI newmodel ()) -> GtkGUI oldmodel void
gtkSetHapplet newHapp init = do
  -- To perform a context switch, we must solve the problem of evaluating a 'GtkGUI' function of one
  -- type (newmodel) within a 'GtkGUI' function of another type (oldmodel). We achieve this by
  -- converting the @'GtkGUI' newmodel@ function into a model-agnostic 'GtkState' function, then we
  -- use this constructed 'GtkState' function to set the 'contextSwticher' field. We then evaluate
  -- 'cancelNow' and allow 'liftGUIintoGtkState' to evaluate the 'contextSwitcher'.
  logIO <- mkLogger "gtkSetHapplet"
  liftIO $ logIO _ctxevt $ "putGUIState -- set context switcher"
  -- Here we set the 'contextSwithcer' field to a function constructed by the 'gtkContextSwitch'
  -- constructor, a function which essentialy converts the polymorphic @'GtkGUI' newmodel@ function
  -- type to a concrete 'GtkState' function type.
  modifyGUIState $ \ guist -> case guist ^. guiWindow of
    GtkUnlockedWin{} -> error "gtkSetHapplet evaluated on unlocked window"
    GtkLockedWin env -> guist & guiWindow .~ GtkLockedWin
      (env & Happlets.Provider.Gtk2.contextSwitcher .~ gtkContextSwitch (env ^. detatchHandler) newHapp init)
  -- Then 'cancelNow' is called, immediately halting evaluation of this function. This ensures
  -- that the 'GUI' function context which called this 'gtkSetHapplet' function will be halted
  -- immediately and control will return to the 'liftGUIintoGtkState' function. It also ensures no
  -- evaluation occurs after 'gtkSetHapplet' is evaluated, which is correct operational semantics
  -- for this function according to the Happlet API.
  liftIO $ logIO _ctxevt "cancelNow"
  cancelNow
  -- We use 'cancelNow' instead of 'deleteEventHandler' because 'disconnectAll' was already called
  -- so we do not want any disconnect function to be evaluated again (which shouldn't cause any
  -- harm, but doing so is an unnnecessary step). It is also for this reason that the
  -- 'liftGUIintoGtkState' function only bothers to evaluate the context switcher when the return
  -- signal is 'HappletEventCancel'. Please refer to the 'liftGUIintoGtkState' function to see
  -- exactly what happens next, after the above 'cancelNow' is evaluated.

-- | This function constructs a 'GtkState' function which performs a context switch. The constructed
-- 'GtkState' function is stored in the 'contextSwitcher' field of the 'GtkWindowState'. Think of
-- this function as a constructor which takes a polymorphic, model-specific 'Happlet' and it's
-- initializer of type @'GtkGUI' newmodel'@ and constructs a function of the concrete,
-- model-agnostic type 'GtkState' which behaves in the exact same way as the 'GtkGUI' function does.
gtkContextSwitch
  :: ConnectReact () -- ^ The 'detatchHandler'.
  -> Happlet newmodel -- ^ the new Happlet which will be used to evaluate the initializer.
  -> (PixSize -> GtkGUI newmodel ()) -- ^ the initializer which will install the new event handlers.
  -> GtkState (EventHandlerControl ())
gtkContextSwitch detatch newHapp init = do
  logIO <- mkLogger "gtkContextSwitch"
  liftIO $ logIO _ctxevt "-- has been set, now executing"
  case detatch of
    ConnectReact{doReact=f} -> logSubGtk logIO _ctxevt "detatchHandler" $ f ()
    Disconnected            -> liftIO $ logIO _ctxevt "-- detatchHandler not set"
  -- Eliminate the prior event handlers.
  disconnectAll logIO
  env <- get
  -- Be very careful to use 'get' AFTER evaluating 'disconnectAll', because 'disconnectAll' is an
  -- important stateful update and we would not want to use a stale @env@, lest the signal
  -- disconnect functions be evaluated more than once.
  --
  -- Now evaluate the new event handler initializer for the new Happlet evaluation context. Note
  -- that we do not use the 'liftGUIintoGtkState' function to perform this task, because
  -- 'liftGUIintoGtkState' is programmed to trigger context switches, and we do not want to trigger
  -- context switcher behavior here in the context switcher.
  (>>= state . const) $ liftIO $ logSubIO logIO _ctxevt "-- evaluate new Happlet initializer" $ do
    size  <- Gtk.widgetGetDrawWindow (gtkWindow env) >>= Gtk.drawableGetSize
    logSubIO logIO _locks "Lock new Happlet that is target of context switch" $
      fmap fst $ onHapplet newHapp $ \ model -> do
        (guiContin, guist) <-
          logSubIO logIO _ctxevt "-- call new Happlet initializer for context switch" $
          runGUI (init $ sampCoord <$> uncurry V2 size) GUIState
            { theGUIProvider = GtkLockedWin env
            , theGUIHapplet = newHapp
            , theGUIModel   = model
            , theGUIWorkers = theGovernment env
            }
        case theGUIWindow guist of
          GtkLockedWin winst -> return ((guiContin, winst), theGUIModel guist)
          GtkUnlockedWin{}   -> fail
            "'evalGUI' was given an unlocked window, but it returned a locked window."

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
      (a, rendst) <- cairoRenderer $ do
        cairoResetAntialiasing
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

instance Managed GtkWindow where
  windowVisible = gtkWindowVisible
  visibleEvents react = installEventHandler EventSetup
    { eventDebugTag     = _winevt
    , eventDescription  = "visibleEvents"
    , eventLensReaction = visibilityReaction
    , eventPreInstall   = return ()
    , eventInstall      = \ logIO sel env next -> do
        let win = gtkWindow env
        logIO (_winevt<>sel) "Glib.on Gtk.visibilityNotifyEvent"
        vis <- Glib.on win Gtk.visibilityNotifyEvent $
          Gtk.eventVisibilityState >>=
          liftIO . next . not . (== Gtk.VisibilityFullyObscured) >>
          return False
        return $ do
          logIO _winevt "Glib.signalDisconnect Gtk.visibilityNotifyEvent"
          Glib.signalDisconnect vis
    , eventGUIReaction  = react
    }
  focusEvents   react = installEventHandler EventSetup
    { eventDebugTag     = _winevt
    , eventDescription  = "focusEvents"
    , eventLensReaction = focusReaction
    , eventPreInstall   = return ()
    , eventInstall      = \ logIO sel env next -> do
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
    , eventGUIReaction  = react
    }

instance CanRecruitWorkers GtkWindow where
  forkGUI f = do
    guist <- getGUIState
    liftIO $ forkIO $ f $ \ gui -> Gtk.postGUISync $ do
      logIO <- mkLogger "inOtherThreadRunGUI"
      -- While a GUI function is evaluating, 'theGUIWindow' is always locked, i.e. set to the
      -- 'GtkLockedWin' state. This is problematic for spawning new threads, because the new thread
      -- will need to share the lock with the thread that spawned it. To solve this problem, a
      -- reference to the lock is stored in the 'thisWindow' pointer. Whenever a new thread is
      -- created, usually when installing an event handler or when launching a 'Worker' thread, the
      -- 'GUIState' used to evaluate the 'GUI' function in the new thread is reset to contain the
      -- 'thisWindow' reference.
      lockedWinRef <- case theGUIWindow guist of
        GtkLockedWin winst -> pure $ GtkUnlockedWin $ thisWindow winst
        GtkUnlockedWin{}   -> error
          "inOtherThreadGUI: was given GUIState containing an unlocked window."
      lockGtkWindow logIO _workers lockedWinRef $ do
        winst <- get
        ((result, winst), _model) <- liftIO $ onHapplet (theGUIHapplet guist) $ \ model -> do
          (result, guist) <- runGUI gui guist
            { theGUIProvider = GtkLockedWin winst
            , theGUIModel  = model
            }
          return ((result, theGUIWindow guist), theGUIModel guist)
        state $ const $ (,) result $! case winst of
          GtkLockedWin winst -> winst
          _                  -> error "runGUI returned unlocked window"
  inOtherThreadRunGUI guist gui = do
    logIO <- mkLogger "inOtherThreadRunGUI"
    -- While a GUI function is evaluating, 'theGUIWindow' is always locked, i.e. set to the
    -- 'GtkLockedWin' state. This is problematic for spawning new threads, because the new thread
    -- will need to share the lock with the thread that spawned it. To solve this problem, a
    -- reference to the lock is stored in the 'thisWindow' pointer. Whenever a new thread is
    -- created, usually when installing an event handler or when launching a 'Worker' thread, the
    -- 'GUIState' used to evaluate the 'GUI' function in the new thread is reset to contain the
    -- 'thisWindow' reference.
    lockedWinRef <- case theGUIWindow guist of
      GtkLockedWin winst -> pure $ GtkUnlockedWin $ thisWindow winst
      GtkUnlockedWin{}   -> error
        "inOtherThreadGUI: was given GUIState containing an unlocked window."
    lockGtkWindow logIO _workers lockedWinRef $ do
      winst <- get
      ((result, winst), _model) <- liftIO $ onHapplet (theGUIHapplet guist) $ \ model -> do
        (result, guist) <- runGUI gui guist
          { theGUIProvider = GtkLockedWin winst
          , theGUIModel  = model
          }
        return ((result, theGUIWindow guist), theGUIModel guist)
      state $ const $ (,) result $! case winst of
        GtkLockedWin winst -> winst
        _                  -> error "runGUI returned unlocked window"
  launchWorkerThread _guist wu cycle getWorker task = do
    logIO <- mkLogger "launchWorkerThread"
    logIO _workers $ "getWorker"
    worker <- getWorker
    logIO _workers $ "getWorker -> "++show worker
    let finish stat = do
          setWorkerStatus worker stat
          logIO _workers $ "retireWorker "++show worker
          retireWorker wu worker
    let timeoutRemove sig = logIO _workers "Gtk.timeoutRemove" >> Gtk.timeoutRemove sig
    let fork stat more t' = do
          let t = round $ (t' :: Double) * 1000
          logIO _workers $ "Gtk.timeoutAdd " ++ show t
          halt <- fmap timeoutRemove $ flip Gtk.timeoutAdd t $ do
            setWorkerStatus worker WorkerBusy
            task worker >>= \ case
              EventHandlerContinue{} -> setWorkerStatus worker WorkerPaused >> return more
              EventHandlerCancel     -> setWorkerStatus worker WorkerPaused >> return more
              EventHandlerHalt       -> finish WorkerHalted >> return False
              EventHandlerFail   msg -> finish (WorkerFailed msg) >> return False
          setWorkerStatus worker stat
          return halt
    logIO _workers $ "unionizeWorker "++show worker
    unionizeWorker wu worker
    liftIO $ case cycle of
      WorkCycleASAP   -> fork WorkerWaiting True 0
      WorkWaitCycle t -> fork WorkerPaused  True t
      WorkCycleWait t -> do
        fork WorkerWaiting False 0 -- Run a single initial cycle with no pause,
        fork WorkerWaiting True  t -- then start the repeating cycle with the requested pause.

instance CanResize GtkWindow where
  resizeEvents mode react = installEventHandler
    (simpleSetup _winevt "resizeEvents" resizeReaction $ uncurry react)
    { eventPreInstall = cairoRenderState . canvasResizeMode .= mode
    }

data AnimationThreadControl
  = AnimationThreadControl
    { animationThreadAlive :: !Bool
    , animationInitTime    :: !UTCTime
    }

instance CanAnimate GtkWindow where
  animationIsRunning = liftGtkStateIntoGUI _animevt "animationIsRunning" $
    gets theAnimatorThread <&> \ case { Disconnected -> False; _ -> True; }
  stepFrameEvents react = do
    installEventHandler EventSetup
      { eventDebugTag     = _animevt
      , eventDescription  = "stepFrameEvents"
      , eventLensReaction = animatorThread
      , eventPreInstall   = return ()
      , eventInstall      = \ logIO sel env next -> do
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
          return $ do
            logIO (_animevt<>sel) "Glib.signalDisconnect (Glib.on timeout)"
            modifyIORef t0ref $ \ ctrl -> ctrl{ animationThreadAlive = False }
            Glib.timeoutRemove frame
      , eventGUIReaction  = react
      }
    -- The 'react' function needs to be evaluated here at time 0, becuase 'timeoutAdd' does not call
    -- it until after the first time step interval has passed. For slow animations, this will cause
    -- a noticable delay between the time the 'stepFrameEvents' function is evaluated and the first
    -- frame callback is evaluated.
    let disconnect msg = liftGtkStateIntoGUI _animevt "stepFrameEvents" $ do
          logIO <- mkLogger "stepFrameEvents"
          forceDisconnect logIO _animevt
            ("-- frame 0 triggered animation halt" ++ msg)
            animatorThread
    guiCatch (react 0) $ \ case
      EventHandlerContinue () -> return ()
      EventHandlerFail    msg -> disconnect (", " ++ Strict.unpack msg)
      EventHandlerCancel      -> disconnect ""
      EventHandlerHalt        -> disconnect ""

instance CanKeyboard GtkWindow where
  keyboardEvents react = installEventHandler EventSetup
    { eventDebugTag     = _keyevt
    , eventDescription  = "keyboardEvents"
    , eventLensReaction = keyHandler
    , eventPreInstall   = return ()
    , eventInstall      = \ logIO sel env next -> do
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
    , eventGUIReaction  = react
    }

instance CanMouse GtkWindow where
  providedMouseDevices = return []
  mouseEvents          = \ case
    MouseButton -> \ react -> installEventHandler EventSetup
      { eventDebugTag     = _mousevt
      , eventDescription  = "mouseEvents MouseButton"
      , eventLensReaction = mouseHandler
      , eventPreInstall   = return ()
      , eventInstall      = \ logIO sel env next -> do
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
      , eventGUIReaction  = react
      }
    MouseDrag   -> \ react -> installEventHandler EventSetup
      { eventDebugTag     = _mousevt
      , eventDescription  = "mouseEvents MouseDrag"
      , eventLensReaction = mouseHandler
      , eventPreInstall   = return ()
      , eventInstall      = \ logIO sel env next -> do
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
      , eventGUIReaction  = react
      }
    MouseAll    -> \ react -> installEventHandler EventSetup
      { eventDebugTag     = _mousevt
      , eventDescription  = "mouseEvents MouseAll"
      , eventLensReaction = mouseHandler
      , eventPreInstall   = return ()
      , eventInstall      = \ logIO sel env next -> do
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
      , eventGUIReaction  = react
      }

----------------------------------------------------------------------------------------------------

-- | When installing a Glib event handler, the sequence of evaluation for locking resources,
-- obtaining parameters from the 'GtkState', evaluating monads using these resources, is a delicate
-- process that is very easy to get wrong. So I have created an abstraction for the sequence of
-- actions that occurs during event handler installation. Event handler installers set the fields
-- of this data structure and pass it to 'intallEventHandler'.
--
-- The 'simpleSetup' constructor is also provided, which is used by event handlers that do not need
-- to install and remove the callbacks because they are always installed, for example the resize
-- window handler. The 'simpleSetup' takes only the debug parameters, the 'eventLensReaction', and the
-- 'eventGUIReaction', and it simply updates the 'eventLensReaction' to trigger the given
-- 'eventGUIReaction'.
data EventSetup event model
  = EventSetup
    { eventDebugTag     :: DebugTag
      -- ^ Which tags to set in the debug system.
    , eventDescription  :: String
      -- ^ A human-readble description of which event handler installer this is, to be reported in
      -- debug logs.
    , eventLensReaction :: Lens' GtkWindowState (ConnectReact event)
      -- ^ A lens that can get and set one of the 'ConnectReact' functions in the Gtk window state.
    , eventPreInstall   :: GtkState ()
      -- ^ An action to perform prior installing the Gtk event handler callback.
    , eventInstall      :: Log IO -> DebugTag -> GtkWindowState -> (event -> IO ()) -> IO (IO ())
      -- ^ Install the Gtk event handler callback which triggers event reactions.
    , eventGUIReaction  :: event -> GtkGUI model ()
      -- ^ The GUI function provided by the end users (programmers) of this library that reacts to
      -- the event with a state update.
    }

-- | An 'EventSetup' that does nothing more than install a 'ConnectReact' function.
simpleSetup
  :: DebugTag
  -> String
  -> Lens' GtkWindowState (ConnectReact event)
  -> (event -> GtkGUI model ())
  -> EventSetup event model
simpleSetup sel what connectReact react = EventSetup
  { eventDebugTag     = sel
  , eventDescription  = what
  , eventLensReaction = connectReact
  , eventPreInstall   = return ()
  , eventInstall      = \ _ _ _ _ -> return $ pure ()
  , eventGUIReaction    = react
  }

-- | This function enables an event handler, and takes a continuation which will be evaluated by
-- this function call to do the low-level work of install the event handler into the Gtk
-- window. This function also does the work of taking a 'Happlets.GUI.GUI' function to be evaluated
-- on each event, converting this function to a 'ConnectReact' function, and installing it into the
-- 'GtkWindow's internal state so that it can be actually evaluated every time the low-level event
-- handler is evaluated. The result is a function that can be used to instantiate any of the
-- "Happlets.GUI" module's event handler classes.
installEventHandler :: Show event => EventSetup event model -> GtkGUI model ()
installEventHandler setup = do
  let sel        = eventDebugTag     setup
  let what       = eventDescription  setup
  let preinstall = eventPreInstall   setup
  let install    = eventInstall      setup
  let react      = eventGUIReaction  setup
  logIO <- mkLogger what
  happ  <- askHapplet
  let dbgmsg = "installEventHandler "++what
  liftGtkStateIntoGUI sel dbgmsg $ do
    forceDisconnect logIO sel what (eventLensReaction setup)
    liftIO $ logIO sel $ "-- preinstall "++what++" event handler"
    preinstall
    env <- get
    disconnect <- liftIO $ do
      logIO sel $ "-- install "++what++"event handler"
      install logIO sel env $ \ event -> do
        lockGtkWindow logIO sel (GtkUnlockedWin $ thisWindow env) $
          evalConnectReact logIO sel what (eventLensReaction setup) event
    liftIO $ logIO sel $ what ++ " -- now enabled"
    (eventLensReaction setup) .= ConnectReact
      { doReact = logSubGtk logIO sel ("liftGUIintoGtkState "++what) .
          liftGUIintoGtkState happ . react >=> \ guiCont ->
            logSubGtk logIO sel (unwords ["checkGUIContinue", what, show guiCont]) $
              checkGUIContinue logIO sel what (eventLensReaction setup) guiCont
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
      Gtk.Super   -> [Super1Key]
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

instance HappletWindow GtkWindow CairoRender where
  getWindowSize             = liftGtkStateIntoGUI _drawevt "getWindowSize" $ do
    logIO <- mkLogger "getWindowSize"
    env <- get
    liftIO $ logWithMVar logIO _drawevt "gtkWindowLive" (gtkWindowLive env) $ \ livest -> do
      -- Obtain the size from the draw window, but will not draw to the draw window.
      (w, h) <- Gtk.drawableGetSize (gtkDrawWindow livest)
      return $ V2 (sampCoord w) (sampCoord h)

  windowChangeHapplet       = gtkSetHapplet

  changeEvents              = installEventHandler .
    simpleSetup _ctxevt "changeEvents" detatchHandler . const

  onCanvas                  =
    liftGtkStateIntoGUI _drawevt "onCanvas"  . evalCairoOnCanvas

  onOSBuffer                =
    liftGtkStateIntoGUI _drawevt "onOSBuffer". evalCairoOnGtkDrawable

  windowClipRegion          = Variable
    { setVal = onCanvas . cairoRender . \ case
        Nothing   -> Cairo.restore
        Just rect -> do
          Cairo.save
          let head = rect ^. rect2DHead
          let tail = rect ^. rect2DTail
          let (x, y) = head ^. pointXY
          let (w, h) = (tail - head) ^. pointXY
          let f (SampCoord x) = realToFrac x
          Cairo.rectangle (f x) (f y) (f w) (f h)
          Cairo.clip
    }

  refreshRegion rects       = liftGtkStateIntoGUI _drawevt "refreshRegion" $ do
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

  refreshWindow            = liftGtkStateIntoGUI _drawevt "refreshWindow" $ do
    logIO <- mkLogger "refreshWindow"
    env <- get
    liftIO $ logWithMVar logIO _drawevt "gtkWindowLive" (gtkWindowLive env) $ \ livest -> do
      logSubIO logIO _drawevt "Gtk.renderWithDrawable refreshWindow" $
        Gtk.renderWithDrawable (gtkDrawWindow livest) $ do
          Cairo.setOperator Cairo.OperatorSource
          Cairo.setSourceSurface (theCairoSurface livest) (0.0) (0.0)
          Cairo.paint

instance Happlet2DGraphics CairoRender where
  pixel p0 = let p@(V2 x y) = realToFrac <$> p in Variable
    { setVal = \ color -> do
        rasterMode x y
        cairoRender $ cairoSetPoint (V2 x y) color
    , getVal = cairoRender (cairoFlush >> cairoGetPoint (V2 x y))
    }

  tempContext = cairoPreserve

  resetGraphicsContext = cairoRender $ do
    cairoFlush
    Cairo.setOperator Cairo.OperatorSource
    cairoResetAntialiasing

  fill = cairoDrawWithSource canvasFillColor Cairo.fill

  stroke = cairoDrawWithSource canvasStrokeColor Cairo.stroke

  blitOperator = Variable
    { setVal = cairoRender . Cairo.setOperator . \ case
        BlitSource   -> Cairo.OperatorSource
        BlitOver     -> Cairo.OperatorOver
        BlitXOR      -> Cairo.OperatorXor
        BlitAdd      -> Cairo.OperatorAdd
        BlitSaturate -> Cairo.OperatorSaturate
    , getVal = cairoRender $ Cairo.getOperator >>= \ case
        Cairo.OperatorSource   -> return BlitSource
        Cairo.OperatorOver     -> return BlitOver
        Cairo.OperatorXor      -> return BlitXOR
        Cairo.OperatorAdd      -> return BlitAdd
        Cairo.OperatorSaturate -> return BlitSaturate
        op -> fail $ "Using a Cairo blit operator not known to Happlets: "++show op
    }

  fillColor = variableFromLens canvasFillColor

  strokeColor = variableFromLens canvasStrokeColor

  clipRegion = Variable
    { setVal = \ rect -> do
        cairoClipRect .= rect
        cairoRender $ Cairo.resetClip >> cairoRectangle (toRational <$> rect) >> Cairo.clip
    , getVal = use cairoClipRect
    }

  clearScreen = unpackRGBA32Color >>> \ (r,g,b,a) -> cairoRender $ cairoClearCanvas r g b a

instance Happlet2DGeometry CairoRender Double where
  shape         = variableFromLens (cairoGeometry . cairoShape)
  strokeWeight  = variableFromLens (cairoGeometry . cairoLineWidth)
  blitTransform = variableFromLens (cairoGeometry . cairoBlitTransform)

-- | This data type contains a pointer to an image buffer in memory, and also a function used to
-- perform some drawing to the pixel values.
newtype CairoPixelBuffer = CairoPixelBuffer{ gtkCairoSurfaceMVar :: MVar Cairo.Surface }

instance CanBufferPixels GtkWindow CairoRender CairoPixelBuffer where
  newImageBuffer size@(V2 w h) draw = liftGtkStateIntoGUI _drawevt "newImageBuffer" $ do
    rendst <- use cairoRenderState
    (a, rendst, img) <- liftIO $ do
      logIO <- mkLogger "newImageBuffer"
      logIO _drawevt $ unwords ["Cairo.createImageSurface Cairo.FormatARGB32", show w, show h]
      surface <- Cairo.createImageSurface Cairo.FormatARGB32 (fromIntegral w) (fromIntegral h)
      (a, rendst) <- logSubIO logIO _drawevt "Cairo.renderWith newImageBuffer" $
        Cairo.renderWith surface $ runCairoRender size rendst draw
      mvar <- newMVar surface
      return (a, rendst, CairoPixelBuffer{gtkCairoSurfaceMVar=mvar})
    cairoRenderState .= rendst
    return (a, img)

  resizeImageBuffer (CairoPixelBuffer{gtkCairoSurfaceMVar=mvar}) size@(V2 w h) draw =
    liftGtkStateIntoGUI _drawevt ("resizeImageBuffer ("++show size++")") $ do
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

  drawImage (CairoPixelBuffer{gtkCairoSurfaceMVar=mvar}) draw = liftGtkStateIntoGUI _drawevt "drawImage" $ do
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
----------------------------------------------------------------------------------------------------

instance AudioPlayback (GUI GtkWindow model) where
  audioPlayback newGen = liftGtkStateIntoGUI _audioevt "audioPlayback" $ do
    oldGen <- use audioPlaybackThread
    let done thid = return $ maybe PCMDeactivated (const PCMActivated) thid
    let switch constr gen = do
          mvar <- liftIO $ newMVar gen
          audioPlaybackThread .= constr Nothing mvar
          return PCMDeactivated
    case (oldGen, newGen) of
      (StereoPlaybackThread thid mvar, PCMGenerateStereo newGen) ->
        liftIO $ swapMVar mvar newGen >> done thid
      (MonoPlaybackThread   thid mvar, PCMGenerateMono   newGen) ->
        liftIO $ swapMVar mvar newGen >> done thid
      (StereoPlaybackThread Nothing _, PCMGenerateMono   newGen) ->
        switch MonoPlaybackThread newGen
      (MonoPlaybackThread   Nothing _, PCMGenerateStereo newGen) ->
        switch StereoPlaybackThread newGen
      (StereoPlaybackThread Just{}  _, PCMGenerateMono   _     ) -> return $ PCMError
        "Audio playback is active in stereo mode, cannot set mono mode PCM generator"
      (MonoPlaybackThread   Just{}  _, PCMGenerateStereo _     ) -> return $ PCMError
        "Audio playback is active in mono mode, cannot set stereo mode PCM generator"

  startupAudioPlayback sizeReq = liftGtkStateIntoGUI _audioevt "startupAudioPlayback" $ do
    let launch
          :: MVar (FrameCounter -> PCM sample)
          -> ((FrameCounter -> PCM sample) -> PCMGenerator)
          -> GtkState PCMActivation
        launch mvar constGen =
          liftIO (readMVar mvar >>= startPlaybackThread sizeReq . constGen) >>=
          assign audioPlaybackThread >>
          return PCMActivated
    let already = return $ PCMError
          "audioPlaybackThread called, audio playback is already activated."
    use audioPlaybackThread >>= \ case
      StereoPlaybackThread Nothing mvar -> launch mvar PCMGenerateStereo
      MonoPlaybackThread   Nothing mvar -> launch mvar PCMGenerateMono
      StereoPlaybackThread Just{}  _    -> already
      MonoPlaybackThread   Just{}  _    -> already

  shutdownAudioPlayback = liftGtkStateIntoGUI _audioevt "shutdownAudioPlayback" $ do
    let shutdown thid = liftIO (killThread thid) >> return PCMDeactivated
    let already = return $ PCMError
          "shutdownAudioPlayback called, audio playback is already deactivated"
    use audioPlaybackThread >>= \ case
      StereoPlaybackThread (Just thid) _ -> shutdown thid
      MonoPlaybackThread   (Just thid) _ -> shutdown thid
      StereoPlaybackThread  Nothing    _ -> already
      MonoPlaybackThread    Nothing    _ -> already

  audioPlaybackState = liftGtkStateIntoGUI _audioevt "audioPlaybackState" $
    ( maybe PCMDeactivated (const PCMActivated) . \ case
        StereoPlaybackThread thid _ -> thid
        MonoPlaybackThread   thid _ -> thid
    ) <$> use audioPlaybackThread

