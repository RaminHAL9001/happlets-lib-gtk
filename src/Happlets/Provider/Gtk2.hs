{-# LANGUAGE CPP #-}

-- | This is the Gtk+ version 2 back-end for Happlets, and will also serve as the Happlet referecne
-- implementation. Creating a window automatically calls the the Gtk+ initializer. Use
-- 'newGtkWindow' to create a new window, and any of the "Happlet.World" functinos to manipulate the
-- windows. This module re-exports the "Happlets" module so it is not necessary to import both.
module Happlets.Provider.Gtk2
  ( Gtk2Provider, gtkHapplet, gtkAnimationFrameRate,
    GtkGUI, GtkState, gtkLaunchEventLoop,

    -- ** Cairo Wrapper Functions
    --
    -- Functions that allow you to call directly into a "Graphics.Rendering.Cairo".'Cairo.Render'
    -- function, but using point, line, and color values specified in the "Happlets.Draw"
    -- sub-modules.
    CairoPixelBuffer, CairoRender, cairoRender,
    --GtkCairoDiagram, gtkCairoDiagram,
    cairoClearCanvas, cairoSetColor, cairoGridLocationOfPoint,
    cairoDrawPath, cairoMoveTo, cairoLineTo, cairoDrawLine,
    cairoDrawRect, cairoPreserve,
    cairoFlush, cairoSetPoint, cairoGetPoint, cairoInvalidate,
    module Happlets,
    module Happlets.View
  )
  where

-- I have found that it is probably better to do double-buffering of the canvas by using a Cairo
-- Surface, however there is a way to make use of a Gtk+ Pixmap as a double-buffer instead. Both
-- ways have been encoded in this source file, because I am not sure which is really the most
-- portable, or which is the most efficient.
#define USE_CAIRO_SURFACE_BUFFER 1

----------------------------------------------------------------------------------------------------

import           Happlets
import           Happlets.Provider.React
import           Happlets.Model.Audio
--import           Happlets.Model.GUI
import           Happlets.View
import           Happlets.View.Audio

import           Happlets.Provider.Cairo      hiding (mkLogger)
import           Happlets.Provider.ALSA
import           Happlets.Provider.Gtk2.Debug

import           Control.Arrow
import           Control.Concurrent

import           Data.Bits
import           Data.IORef
import qualified Data.Map            as Map
import           Data.Maybe
import qualified Data.Text           as Strict
import           Data.Time.Clock

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
  liftIO . logSubIO logIO sel msg . runProviderIO f >>= state . const
{-# INLINE logSubGtk #-}

----------------------------------------------------------------------------------------------------

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

data Gtk2Provider
  = Gtk2Provider
    { currentConfig           :: !HappletInitConfig
    , thisWindow              :: !(ProviderStateLock Gtk2Provider)
      -- ^ While a GUI function is evaluating, 'theGUIProvider' is always locked, i.e. set to the
      -- 'GtkLockedWin' state. This is problematic for spawning new threads, because the new thread
      -- will need to share the lock with the thread that spawned it. To solve this problem, a
      -- reference to the lock is stored in the 'thisWindow' pointer. Whenever a new thread is
      -- created, usually when installing an event handler or when launching a 'Worker' thread, the
      -- 'GUIState' used to evaluate the 'GUI' function in the new thread is reset to contain the
      -- 'thisWindow' reference.
    , gtkWindowLive           :: !(MVar GtkWindowLive) -- TODO: store live window as-is, no MVar
    , gtkWindow               :: !Gtk.Window
    , theGtkEventBox          :: !Gtk.EventBox
    , theCairoRenderState     :: !CairoRenderState
    , theAudioPlaybackThread  :: !AudioPlaybackThread
    , theInitReaction         :: !(ConnectReact GtkState PixSize)
    , theResizeReaction       :: !(ConnectReact GtkState (OldPixSize, NewPixSize))
    , theVisibilityReaction   :: !(ConnectReact GtkState Bool)
    , theFocusReaction        :: !(ConnectReact GtkState Bool)
    , theMouseHandler         :: !(ConnectReact GtkState Mouse)
    , theKeyHandler           :: !(ConnectReact GtkState Keyboard)
    , theDetatchHandler       :: !(ConnectReact GtkState ())
    , theAnimatorThread       :: !(ConnectReact GtkState UTCTime)
    , theContextSwitcher      :: !(GtkState (Consequence ()))
    }

instance MonadProvider Gtk2Provider GtkState where
  runProviderIO (GtkState f) = runStateT f
  contextSwitcher = lens theContextSwitcher $ \ a b -> a{ theContextSwitcher = b }
  providerSharedState = thisWindow
  getProviderWindowSize = do
    logIO <- mkLogger "getWindowSize"
    env <- get
    liftIO $ logWithMVar logIO _drawevt "gtkWindowLive" (gtkWindowLive env) $ \ livest -> do
      -- Obtain the size from the draw window, but will not draw to the draw window.
      (w, h) <- Gtk.drawableGetSize (gtkDrawWindow livest)
      return $ V2 (sampCoord w) (sampCoord h)
  signalFatalError = liftIO . hPutStrLn stderr . Strict.unpack
  initReaction = lens theInitReaction $ \ a b -> a{ theInitReaction = b }
  detatchHandler = lens theDetatchHandler $ \ a b -> a{ theDetatchHandler = b }
  disconnectAll = do
    forceDisconnect visibilityReaction
    forceDisconnect focusReaction
    forceDisconnect mouseHandler
    forceDisconnect keyHandler
    forceDisconnect animatorThread
    forceDisconnect detatchHandler
    forceDisconnect resizeReaction

gtkEventBox :: Lens' Gtk2Provider Gtk.EventBox
gtkEventBox = lens theGtkEventBox $ \ a b -> a{ theGtkEventBox = b }

cairoRenderState :: Lens' Gtk2Provider CairoRenderState
cairoRenderState = lens theCairoRenderState $ \ a b -> a{ theCairoRenderState = b }

audioPlaybackThread :: Lens' Gtk2Provider AudioPlaybackThread
audioPlaybackThread = lens theAudioPlaybackThread $ \ a b -> a{ theAudioPlaybackThread = b }

gtkCurrentWindowSize :: Lens' Gtk2Provider PixSize
gtkCurrentWindowSize = cairoRenderState . cairoKeepWinSize

visibilityReaction :: Lens' Gtk2Provider (ConnectReact GtkState Bool)
visibilityReaction = lens theVisibilityReaction $ \ a b -> a{ theVisibilityReaction = b }

focusReaction :: Lens' Gtk2Provider (ConnectReact GtkState Bool)
focusReaction = lens theFocusReaction $ \ a b -> a{ theFocusReaction = b }

resizeReaction :: Lens' Gtk2Provider (ConnectReact GtkState (OldPixSize, NewPixSize))
resizeReaction = lens theResizeReaction $ \ a b -> a{ theResizeReaction = b }

mouseHandler :: Lens' Gtk2Provider (ConnectReact GtkState Mouse)
mouseHandler = lens theMouseHandler $ \ a b -> a{ theMouseHandler = b }

keyHandler :: Lens' Gtk2Provider (ConnectReact GtkState Keyboard)
keyHandler = lens theKeyHandler $ \ a b -> a{ theKeyHandler = b }

---- When a context switch is performed, this value is set with a continuation to be called after the
---- 'windowChangeHapplet' function completes. After evaluation of the 'GtkGUI' function completes,
---- the state is evaluated -- every single event handler will evaluate this function. To ensure that
---- nothing happens unless it is set, this lens is used to set the callback to @return ()@ (a no-op)
---- prior to evaluation of the 'GtkGUI' procedure in the 'liftGUIintoGtkState' function.
--gtk2ContextSwitcher :: Lens' Gtk2Provider (GtkState (Consequence ()))
--gtk2ContextSwitcher = lens theContextSwitcher $ \ a b -> a{ theContextSwitcher = b }

animatorThread :: Lens' Gtk2Provider (ConnectReact GtkState UTCTime)
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
  = GtkState { unwrapGtkState :: StateT Gtk2Provider IO a }
  deriving (Functor)

instance Applicative GtkState where
  pure = return
  (<*>) = ap

instance Monad GtkState where
  return = GtkState . return
  (GtkState a) >>= b = GtkState $ a >>= unwrapGtkState . b

instance MonadState Gtk2Provider GtkState where
  state = GtkState . state

instance MonadIO GtkState where
  liftIO = GtkState . liftIO

-- | Create the window and install the permanent event handlers.
gtkNewWindow :: HappletInitConfig -> IO (ProviderStateLock Gtk2Provider)
gtkNewWindow cfg = do
  logIO <- mkLogger "createWin"
  forM_ (initConfigErrorsOnLoad cfg) (hPrint stderr)
  -- new window --
  logIO _setup $ "Gtk.windowNew"
  window <- Gtk.windowNew
  logIO _setup $ "Gtk.widgetSetHasWindow"
  Gtk.widgetSetHasWindow window True
  logIO _setup $ "Gtk.widgetSetAppPaintable"
  Gtk.widgetSetAppPaintable window True
  logIO _setup $ "Gtk.widgetSetDoubleBuffered window False"
  Gtk.widgetSetDoubleBuffered window False
  when (isJust $ cfg ^. initBackgroundTransparency) $
    mkAlphaChannel logIO $ Gtk.castToWidget window
  logIO _setup $ "Gtk.windowSetTypeHint GtkWindowTtypeHintNormal"
  Gtk.windowSetTypeHint window Gtk.WindowTypeHintNormal
  logIO _setup $ "Gtk.windowSetDefaultSize " ++ show (cfg ^. recommendWindowSize)
  uncurry (Gtk.windowSetDefaultSize window) $ cfg ^. recommendWindowSize
  logIO _setup $ "Gtk.windowSetDecorated " ++ show (cfg ^. initDecorateWindow)
  Gtk.windowSetDecorated window $ cfg ^. initDecorateWindow
  eventBox <- Gtk.eventBoxNew
  Gtk.eventBoxSetVisibleWindow eventBox False
  Gtk.eventBoxSetAboveChild    eventBox True
  logIO _setup $ "Gtk.containerAdd window eventBox"
  Gtk.containerAdd window eventBox
  live  <- newEmptyMVar
  --gov   <- newWorkerUnion
  audio <- newMVar (const $ return (0, 0))
  initProviderState $ \ this -> do
    let env = Gtk2Provider
          { currentConfig            = cfg
          , thisWindow               = this
          , gtkWindowLive            = live
          , gtkWindow                = window
          , theGtkEventBox           = eventBox
          , theCairoRenderState      = CairoRenderState
              { theCairoKeepWinSize    = V2 0 0
              , theCairoRenderMode     = VectorMode
              , theCanvasResizeMode    = CanvasResizeClear
              , theGtkCairoSurface     = Nothing
              , theCairoViewBounds     = rect2D
              , theCairoScreenPrinterState = screenPrinterState
              }
          , theAudioPlaybackThread   = StereoPlaybackThread Nothing audio
          -- , theGovernment            = gov
          , theInitReaction          = Disconnected
          , theResizeReaction        = Disconnected
          , theVisibilityReaction    = Disconnected
          , theFocusReaction         = Disconnected
          , theMouseHandler          = Disconnected
          , theKeyHandler            = Disconnected
          , theDetatchHandler        = Disconnected
          , theAnimatorThread        = Disconnected
          , theContextSwitcher       = return (ActionOK ())
          }
    ((), env) <- flip runProviderIO env $ do
      installExposeEventHandler
      installDeleteEventHandler
      installInitEventHandler
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
-- 'Gtk2Provider' and evaluating it, then installing a configure event handler. 
installInitEventHandler :: GtkState ()
installInitEventHandler = do
  logIO <- mkLogger "permanentHandlers"
  env <- get
  liftIO $ do
    ------------------------------------ Initializing Event --------------------------------
    logIO _setup "Glib.on Gtk.configureEvent -- for init handler"
    initHandler <- newEmptyMVar
    (>>= (putMVar initHandler)) $ Glib.on (gtkWindow env) Gtk.configureEvent $ do
      -- The 'Glib.on' function ensures that this 'do' block will evaluated in a separate thread.
      logIO <- mkLogger "initCallback"
      canvas <- Gtk.eventWindow
      size   <- Gtk.eventSize
      let allocSize = dimsForAlloc size
      let evt = sampCoord <$> uncurry V2 size
      -- It is ok to use 'runProviderOnLock' here, because this code is evaluated in some other
      -- thread created to handle the event when it occurs.
      liftIO $ runProviderOnLock (thisWindow env) $ do
        live   <- gets gtkWindowLive
        liftIO $ do
          logIO _setup $
            "newGtkWindowLive (dimsForAlloc " ++ show size ++ " --> " ++ show allocSize ++ ")"
          livest <- newGtkWindowLive (currentConfig env) canvas allocSize
          logIO _setup "putMVar gtkWindowLive"
          putMVar live livest
          logIO _setup $ "initReaction " ++ show size
        --evalConnectReact logIO _setup "initReaction" initReaction evt
        evalConnectReact initReaction evt
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
    -- The 'Glib.on' function ensures that this 'do' block will evaluated in a separate thread.
    void $ Glib.on (gtkWindow env) Gtk.configureEvent $ do
      logIO  <- mkLogger "configureEventCallback"
      canvas <- Gtk.eventWindow
      size   <- Gtk.eventSize
      let evt = (env ^. gtkCurrentWindowSize, sampCoord <$> uncurry V2 size)
      -- It is ok to use 'otherThreadRunProviderIO' here, because this code does evaluate in some
      -- other thread.
      liftIO $ runProviderOnLock (thisWindow env) $ do
        logSubGtk logIO _winevt ("resizeGtkDrawContext "++show size) $
          resizeGtkDrawContext canvas size
        --evalConnectReact logIO _winevt "resizeReaction" resizeReaction evt
        evalConnectReact resizeReaction evt
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
    -- The 'Glib.on' function ensures that this 'do' block will evaluated in a separate thread.
    Glib.on (gtkWindow env) Gtk.deleteEvent $
      -- It is ok to use 'otherThreadRunProviderIO' here, because there is no associated
      -- 'ConnectReact' function installed into the 'GtkState', and this function is not installed
      -- by the ordinary 'installEventHandler' function that takes care of locking for us.
      liftIO $ runProviderOnLock (thisWindow env) $ do
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

--deleteWin :: Gtk2Provider -> IO ()
--deleteWin env = do
--  logIO <- mkLogger "deleteWin"
--  logIO _winevt "Gtk.widgetDestroy"
--  Gtk.widgetDestroy (gtkWindow env)

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
newGtkWindowLive :: HappletInitConfig -> Gtk.DrawWindow -> (Int, Int) -> IO GtkWindowLive
newGtkWindowLive cfg canvas (w, h) = do
  logIO <- mkLogger "gtkAllocNewPixmap"
#if USE_CAIRO_SURFACE_BUFFER
  let depth = if isJust $ cfg ^. initBackgroundTransparency
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

----------------------------------------------------------------------------------------------------

-- | This is a type of 'Happlets.GUI.GUI' function where the @window@ type is 'GtkWindow' and the
-- @draw@ type is 'GtkDraw' (a Cairo 'Graphics.Rendering.Cairo.Render' function).
type GtkGUI model = GUI Gtk2Provider model

gtkWindowVisible :: ConfigState (GtkGUI model) Bool
gtkWindowVisible = ConfigState
  { setConfig = \ visible -> liftGUIProvider $ gets gtkWindow >>= \ win -> liftIO $
      if not visible then Gtk.widgetHideAll win else
      Gtk.widgetShowAll win >> Gtk.widgetQueueDraw win
  , getConfig = liftGUIProvider $ gets gtkWindow >>= liftIO . Gtk.widgetGetVisible
  }

-- | This is the function behind the Happlet public API 'Happlets.Initialize.attachWindow'.
gtkAttachHapplet
  :: Bool
  -> ProviderStateLock Gtk2Provider
  -> Happlet model
  -> (PixSize -> GtkGUI model ())
  -> IO ()
gtkAttachHapplet showWin win happ init = void $ runProviderOnLock win $ do
  disconnectAll
  initReaction .= ConnectReact
    { doDisconnect = return () -- no need, is disconnected automatically by the init callback.
    , reactTo = \ size -> do
        eventControl <- providerLiftGUI happ $ init size
        case eventControl of
          ActionOK ()       -> do
            env <- get
            when showWin $ liftIO $ do
              Gtk.widgetShowAll (gtkWindow env)
              Gtk.widgetQueueDraw (gtkWindow env)
          ActionHalt        -> liftIO $ hPutStrLn stderr $
            "'deleteEventHandler' was called during Happlet initialization.\n" ++
            "Happlet will not attach to window."
          ActionCancel      -> liftIO $ hPutStrLn stderr $
            "'cancelNow' was called during Happlet initialization.\n"++
            "Happlet will not attach to window."
          ActionFail   msg  -> liftIO $ hPutStrLn stderr $
            "Happlet failed to initialize: "++show msg
            -- TODO: also show a modal dialog box reporting the error.
    }
  win <- gets gtkWindow
  liftIO $ Gtk.widgetShow win

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

-- Cairo rendering function evaluation.

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

----------------------------------------------------------------------------------------------------

instance Managed Gtk2Provider where
  windowVisible = gtkWindowVisible
  visibleEvents react = installEventHandler EventSetup
    { --eventDebugTag     = _winevt,
      eventDescription  = "visibleEvents"
    , eventLensReaction = visibilityReaction
    , eventPreInstall   = return ()
    , eventInstall      = \ env next -> do
        let win = gtkWindow env
        vis <- Glib.on win Gtk.visibilityNotifyEvent $
          Gtk.eventVisibilityState >>=
          liftIO . next . not . (== Gtk.VisibilityFullyObscured) >>
          return False
        return $ Glib.signalDisconnect vis
    , eventGUIReaction  = react
    }
  focusEvents   react = installEventHandler EventSetup
    { --eventDebugTag     = _winevt,
      eventDescription  = "focusEvents"
    , eventLensReaction = focusReaction
    , eventPreInstall   = return ()
    , eventInstall      = \ env next -> do
        let win = gtkWindow env
        focin  <- Glib.on win Gtk.focusInEvent  $ liftIO (next True)  >> return False
        focout <- Glib.on win Gtk.focusOutEvent $ liftIO (next False) >> return False
        return $ Glib.signalDisconnect focin >> Glib.signalDisconnect focout
    , eventGUIReaction  = react
    }
  windowSize = ConfigState
    { getConfig = liftGUIProvider $ do
        win <- gets gtkWindow
        liftIO $ do
          (x, y) <- Gtk.windowGetPosition win
          (w, h) <- Gtk.windowGetSize     win
          return $ rect2D
            & rect2DTail . pointXY .~ (sampCoord x, sampCoord y)
            & rect2DHead . pointXY .~ (sampCoord x + sampCoord w, sampCoord y + sampCoord h)
    , setConfig = \ rect -> liftGUIProvider $ do
        win <- gets gtkWindow
        liftIO $ do
          let f = fromIntegral :: SampCoord -> Int
          let (x , y ) = rect ^. rect2DTail . pointXY
          let (x1, y1) = rect ^. rect2DHead . pointXY
          let (w , h ) = (x1 - x, y1 - y)
          Gtk.windowMove   win (f x) (f y)
          Gtk.windowResize win (f w) (f h)
    }
  windowDecorated = ConfigState
    { getConfig = liftGUIProvider $ do
        win <- gets gtkWindow
        liftIO $ Gtk.windowGetDecorated win
    , setConfig = \ bool -> liftGUIProvider $ do
        win <- gets gtkWindow
        liftIO $ Gtk.windowSetDecorated win bool
    }

instance ProviderSyncCallback Gtk2Provider GtkState where
  providerRunSync lock f = Gtk.postGUISync $ runProviderOnLock lock f 

instance CanResize Gtk2Provider where
  resizeEvents mode react = installEventHandler
    (simpleSetup resizeReaction $ uncurry react)
    { eventPreInstall = cairoRenderState . canvasResizeMode .= mode
    }
  windowResizeMode = fmapConfigStateMonad liftGUIProvider $
    configStateWithLens (cairoRenderState . canvasResizeMode)

instance CanAnimate Gtk2Provider where
  animationIsRunning = liftGUIProvider $
    gets theAnimatorThread <&> \ case { Disconnected -> False; _ -> True; }
  stepFrameEvents react = do
    installEventHandler EventSetup
      {
      --eventDebugTag     = _animevt
        eventDescription  = "stepFrameEvents"
      , eventLensReaction = animatorThread
      , eventPreInstall   = return ()
      , eventInstall      = \ env next ->
          let rate = env & view animationFrameRate . currentConfig in
          Glib.timeoutRemove <$>
          Glib.timeoutAdd
          (getCurrentTime >>= next >> return True)
          (min 200 $ floor (1000.0 / rate))
      , eventGUIReaction  = react
      }
    -- The 'react' function needs to be evaluated here at time 0, becuase 'timeoutAdd' does not call
    -- it until after the first time step interval has passed. For slow animations, this will cause
    -- a noticable delay between the time the 'stepFrameEvents' function is evaluated and the first
    -- frame callback is evaluated.
    let disconnect = liftGUIProvider $ forceDisconnect animatorThread
    guiCatch (liftIO getCurrentTime >>= react) $ \ case
      ActionOK ()      -> return ()
      ActionFail  _msg -> disconnect
      ActionCancel     -> disconnect
      ActionHalt       -> disconnect

instance CanKeyboard Gtk2Provider where
  keyboardEvents react = installEventHandler EventSetup
    {
    --eventDebugTag     = _keyevt
      eventDescription  = "keyboardEvents"
    , eventLensReaction = keyHandler
    , eventPreInstall   = return ()
    , eventInstall      = \ env next -> do
      -- Event boxes do not capture key events for some reason. Key handlers must be installed into
      -- the window widget itself.
      let box = gtkWindow env
      press   <- Glib.on box Gtk.keyReleaseEvent $
        handleKey False >>= liftIO . next >> return False
      release <- Glib.on box Gtk.keyPressEvent $
        handleKey True  >>= liftIO . next >> return False
      return $ Glib.signalDisconnect press >> Glib.signalDisconnect release
    , eventGUIReaction  = react
    }

instance CanMouse Gtk2Provider where
  providedMouseDevices = return []
  mouseEvents          = \ case
    MouseButton -> \ react -> installEventHandler EventSetup
      {
      --eventDebugTag     = _mousevt
        eventDescription  = "mouseEvents MouseButton"
      , eventLensReaction = mouseHandler
      , eventPreInstall   = return ()
      , eventInstall      = \ env next -> do
          let box = env ^. gtkEventBox
          press <- Glib.on box Gtk.buttonPressEvent $
            handleMouse True >>= liftIO . next >> return False
          release <- Glib.on box Gtk.buttonReleaseEvent $
            handleMouse False >>= liftIO . next >> return False
          return $ Glib.signalDisconnect press >> Glib.signalDisconnect release
      , eventGUIReaction  = react
      }
    MouseDrag   -> \ react -> installEventHandler EventSetup
      {
      --eventDebugTag     = _mousevt
        eventDescription  = "mouseEvents MouseDrag"
      , eventLensReaction = mouseHandler
      , eventPreInstall   = return ()
      , eventInstall      = \ env next -> do
          let box = env ^. gtkEventBox
          press <- Glib.on box Gtk.buttonPressEvent $
            handleMouse True >>= liftIO . next >> return False
          release <- Glib.on box Gtk.buttonReleaseEvent $
            handleMouse False >>= liftIO . next >> return False
          button <- Glib.on box Gtk.motionNotifyEvent $
            handleCursor True >>= liftIO . next >> return False
          Gtk.widgetAddEvents box [Gtk.ButtonMotionMask]
          return $ do
            Glib.signalDisconnect press
            Glib.signalDisconnect release
            Glib.signalDisconnect button
            -- Gtk has an assertion which disallows calling 'Gtk.widgetDelEvents' while a widget is
            -- realized. In order to remove the event mask, we must first remove the widget from the
            -- window container, then add it back again.
            Gtk.containerRemove (gtkWindow env) box
            Gtk.widgetDelEvents box [Gtk.ButtonMotionMask]
            Gtk.containerAdd (gtkWindow env) box
      , eventGUIReaction  = react
      }
    MouseAll    -> \ react -> installEventHandler EventSetup
      {
      --eventDebugTag     = _mousevt
        eventDescription  = "mouseEvents MouseAll"
      , eventLensReaction = mouseHandler
      , eventPreInstall   = return ()
      , eventInstall      = \ env next -> do
          let box = env ^. gtkEventBox
          button  <- newIORef False
          press   <- Glib.on box Gtk.buttonPressEvent   $
            liftIO (writeIORef button True) >>
            handleMouse True  >>= liftIO . next >> return False
          release <- Glib.on box Gtk.buttonReleaseEvent $
            liftIO (writeIORef button False) >>
            handleMouse False >>= liftIO . next >> return False
          motion  <- Glib.on box Gtk.motionNotifyEvent  $
            liftIO (readIORef button) >>=
            handleCursor >>= liftIO . next >> return False
          Gtk.widgetAddEvents box [Gtk.PointerMotionMask, Gtk.ButtonMotionMask]
          return $ do
            Glib.signalDisconnect press
            Glib.signalDisconnect release
            Glib.signalDisconnect motion
            -- Gtk has an assertion which disallows calling 'Gtk.widgetDelEvents' while a widget is
            -- realized. In order to remove the event mask, we must first remove the widget from the
            -- window container, then add it back again.
            Gtk.containerRemove (gtkWindow env) box
            Gtk.widgetDelEvents box [Gtk.PointerMotionMask, Gtk.ButtonMotionMask]
            Gtk.containerAdd (gtkWindow env) box
      , eventGUIReaction  = react
      }

----------------------------------------------------------------------------------------------------

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

instance HappletWindow Gtk2Provider CairoRender where
  --getWindowSize             = liftGtkStateIntoGUI _drawevt "getWindowSize" $ do
  --  logIO <- mkLogger "getWindowSize"
  --  env <- get
  --  liftIO $ logWithMVar logIO _drawevt "gtkWindowLive" (gtkWindowLive env) $ \ livest -> do
  --    -- Obtain the size from the draw window, but will not draw to the draw window.
  --    (w, h) <- Gtk.drawableGetSize (gtkDrawWindow livest)
  --    return $ V2 (sampCoord w) (sampCoord h)

  --windowChangeHapplet       = gtkSetHapplet

  changeEvents              = installEventHandler . simpleSetup detatchHandler . const

  onCanvas                  = liftGUIProvider . evalCairoOnCanvas

  onOSBuffer                = liftGUIProvider . evalCairoOnGtkDrawable

  windowClipRegion          = ConfigState
    { getConfig = onCanvas $ CairoRender $ Just <$> use cairoViewBounds
    , setConfig = \ case
        Nothing   -> do
          winsize <- liftGUIProvider $ do
            livewin <- gets gtkWindowLive
            liftIO $ withMVar livewin $
              fmap (($ point2D) . (pointXY .~) . (fromIntegral *** fromIntegral)) .
              Gtk.drawableGetSize . gtkDrawWindow
          onCanvas $ do
            cairoViewBounds .= (rect2D & rect2DHead .~ winsize)
            cairoRender Cairo.restore
        Just irect -> onCanvas $ do
          let rect = realToFrac <$> irect :: Rect2D Double
          cairoViewBounds .= irect
          cairoRender $ do
            Cairo.save
            let tail = rect ^. rect2DTail
            let head = rect ^. rect2DHead
            let (x, y) = tail ^. pointXY
            let (w, h) = (head - tail) ^. pointXY
            Cairo.rectangle x y w h
            Cairo.clip
    }

  refreshRegion rects       = liftGUIProvider $ do
    logIO <- mkLogger "refreshRegion"
    env <- get
    liftIO $ logWithMVar logIO _drawevt "gtkWindowLive" (gtkWindowLive env) $ \ livest ->
      logSubIO logIO _drawevt "Gtk.renderWithDrawable refreshRegion" $
        Gtk.renderWithDrawable (gtkDrawWindow livest) $
          forM_ (fmap realToFrac . canonicalize2DShape <$> rects) $ \ rect -> do
            Cairo.setOperator Cairo.OperatorSource
            Cairo.setSourceSurface (theCairoSurface livest) (0.0) (0.0)
            let (x, y) = rect ^. rect2DTail . pointXY
            let (w, h) = ((rect ^. rect2DHead) - (rect ^. rect2DTail)) ^. pointXY
            Cairo.rectangle x y w h
            Cairo.fill

  refreshWindow            = liftGUIProvider $ do
    logIO <- mkLogger "refreshWindow"
    env <- get
    liftIO $ logWithMVar logIO _drawevt "gtkWindowLive" (gtkWindowLive env) $ \ livest -> do
      logSubIO logIO _drawevt "Gtk.renderWithDrawable refreshWindow" $
        Gtk.renderWithDrawable (gtkDrawWindow livest) $ do
          Cairo.setOperator Cairo.OperatorSource
          Cairo.setSourceSurface (theCairoSurface livest) (0.0) (0.0)
          Cairo.paint

instance HappletPixelBuffer Gtk2Provider CairoRender where
  imageCanvasResizeMode = ConfigState
    { setConfig = \ mode -> onCanvas $ canvasResizeMode .= mode
    , getConfig = onCanvas $ use canvasResizeMode
    }

  resizeImageBuffer size@(V2 w h) redraw = onCanvas $ do
    let newSurface = liftIO $
          Cairo.createImageSurface Cairo.FormatARGB32 (fromIntegral w) (fromIntegral h)
    rendst <- get
    csurf  <- case rendst ^. gtkCairoSurface of
      Nothing    -> newSurface
      Just csurf -> do
        oldDims <- V2
          <$> Cairo.imageSurfaceGetWidth  csurf
          <*> Cairo.imageSurfaceGetHeight csurf
        if (sampCoord <$> oldDims) == size then return csurf else newSurface
    (a, rendst) <- liftIO $ Cairo.renderWith csurf $ runCairoRender size rendst redraw
    put rendst
    gtkCairoSurface .= Just csurf
    return a

----------------------------------------------------------------------------------------------------

instance AudioPlayback (GUI Gtk2Provider model) where
  audioPlayback newGen = liftGUIProvider $ do
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

  startupAudioPlayback sizeReq = liftGUIProvider $ do
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

  shutdownAudioPlayback = liftGUIProvider $ do
    let shutdown thid = liftIO (killThread thid) >> return PCMDeactivated
    let already = return $ PCMError
          "shutdownAudioPlayback called, audio playback is already deactivated"
    use audioPlaybackThread >>= \ case
      StereoPlaybackThread (Just thid) _ -> shutdown thid
      MonoPlaybackThread   (Just thid) _ -> shutdown thid
      StereoPlaybackThread  Nothing    _ -> already
      MonoPlaybackThread    Nothing    _ -> already

  audioPlaybackState = liftGUIProvider $
    ( maybe PCMDeactivated (const PCMActivated) . \ case
        StereoPlaybackThread thid _ -> thid
        MonoPlaybackThread   thid _ -> thid
    ) <$> use audioPlaybackThread

----------------------------------------------------------------------------------------------------

-- | This is the Happlet back-end 'Happlets.Provider.Provider' which you must pass to
-- 'Happlets.GUI.runGUI' in the @main@ function of your Happlet program.
gtkHapplet :: Provider Gtk2Provider
gtkHapplet = Provider
  { defaultConfig = happletInitConfig &~ do
      initConfigFilePath         .= ""
      registeredAppName          .= "Happlet"
      initWindowTitleBar         .= "Happlet"
      initBackgroundTransparency .= Just 0.9
      initBackgroundGreyValue    .= 1.0
      recommendWindowPosition    .= (0, 30)
      recommendWindowSize        .= (800, 600)
      animationFrameRate         .= gtkAnimationFrameRate
      initDecorateWindow         .= True
      quitOnWindowClose          .= True
      deleteWindowOnClose        .= True
  , doInitializeGUI         = gtkInit
  , doGUIEventLoopLaunch    = gtkLaunchEventLoop
  , doProviderNew           = gtkNewWindow
  , doProviderDelete        = flip runProviderOnLock $ gets gtkWindow >>= liftIO . Gtk.widgetDestroy
  , doProviderAttach        = gtkAttachHapplet
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
gtkLaunchEventLoop :: HappletInitConfig -> IO ()
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
