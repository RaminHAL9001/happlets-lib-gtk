-- | This is the Gtk+ version 2 back-end for Happlets, and will also serve as the Happlet referecne
-- implementation. Creating a window automatically calls the the Gtk+ initializer. Use
-- 'newGtkWindow' to create a new window, and any of the "Happlet.World" functinos to manipulate the
-- windows. This module re-exports the "Happlets" module so it is not necessary to import both.
module Happlets.Lib.Gtk
  ( gtkHapplet, GtkGUI, GtkRedraw,
    GtkWindow, gtkLaunchEventLoop,
    GtkImage, CairoRender(..), GtkCairoDiagram, gtkCairoDiagram,
    cairoClearCanvas, cairoSetColorRGBA32,
    gtkAnimationFrameRate,
    module Happlets,
    module Happlets.Draw
  )
  where

----------------------------------------------------------------------------------------------------

import           Happlets
import           Happlets.Draw
import           Happlets.Provider

import           Control.Arrow
import           Control.Concurrent
--import           Control.Lens
--import           Control.Monad
--import           Control.Monad.Reader
import           Control.Monad.State

import           Data.IORef
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Text         as Strict
import           Data.Time.Clock
--import           Data.Typeable
import           Data.Word

import qualified Graphics.Rendering.Cairo           as Cairo

--import qualified Graphics.UI.Gtk.Abstract.Container as Gtk
import qualified Graphics.UI.Gtk.Abstract.Widget    as Gtk
import qualified Graphics.UI.Gtk.Cairo              as Gtk
--import qualified Graphics.UI.Gtk.Display.Image      as Gtk
--import qualified Graphics.UI.Gtk.Gdk.GtkWindowState         as Gtk
import qualified Graphics.UI.Gtk.Gdk.Drawable       as Gtk
import qualified Graphics.UI.Gtk.Gdk.DrawWindow     as Gtk
import qualified Graphics.UI.Gtk.Gdk.EventM         as Gtk
--import qualified Graphics.UI.Gtk.Gdk.Keys           as Gtk
import qualified Graphics.UI.Gtk.Gdk.GC             as Gtk
import qualified Graphics.UI.Gtk.Gdk.Pixmap         as Gtk
import qualified Graphics.UI.Gtk.Gdk.Region         as Gtk
import qualified Graphics.UI.Gtk.Gdk.Screen         as Gtk
import qualified Graphics.UI.Gtk.General.General    as Gtk
--import qualified Graphics.UI.Gtk.Layout.Alignment   as Gtk
import qualified Graphics.UI.Gtk.Windows.Window     as Gtk

import           Diagrams.Backend.Cairo.Internal
import           Diagrams.BoundingBox
import           Diagrams.Core.Compile
import           Diagrams.Core.Types
import           Diagrams.Size                    (dims)

import           Linear.Affine
import           Linear.V2 (V2(..))

import           System.IO
import           System.IO.Unsafe

--import           System.Glib.Attributes (AttrOp(..))
--import qualified System.Glib.Attributes             as Glib
import qualified System.Glib.Signals                as Glib
import qualified System.Glib.Utils                  as Glib
import qualified System.Glib.MainLoop               as Glib

import           Debug.Trace

----------------------------------------------------------------------------------------------------

debugThisModule :: Bool
debugThisModule = True

type LogGUI = String -> IO ()

mkLogger
  :: (Monad m, MonadIO m)
  => String -> Bool -> m LogGUI
mkLogger func enable = return $ if not (debugThisModule && enable)
  then const $ return ()
  else \ msg -> do
    tid <- myThreadId
    traceIO $ '[' : show tid ++ "][Happlets.Lib.Gtk." ++ func ++ "] " ++ msg

----------------------------------------------------------------------------------------------------

dToW16 :: Double -> Word16
dToW16 = round . (*) (realToFrac (maxBound::Word16))

dToGrey :: Double -> Gtk.Color
dToGrey d = Gtk.Color (dToW16 d) (dToW16 d) (dToW16 d)

-- | This is the frame rate used by default when installing an animation event handler. This value
-- can be configured by modifying the 'Happlets.Config.animationFrameRate'.
gtkAnimationFrameRate :: Double
gtkAnimationFrameRate = 60.0

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

-- | A monadic wrapper around a 'Cario.Render' monad. The only reason for this to exist is because
-- there needs to be an instance of 'Data.Semigroup.Semigroup'.
--
-- The 'Controller' type is defined to use the 'CairoRender' as it's @view@ type. You can also
-- convert a 'GtkCairoDiagram' to a 'CairoRender' using the 'gtkCairoDiagram' function.
newtype CairoRender a = CairoRender { runCairoRender :: Cairo.Render a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Semigroup a => Semigroup (CairoRender a) where
  (CairoRender a) <> (CairoRender b) = CairoRender $ (<>) <$> a <*> b

instance Monoid a => Monoid (CairoRender a) where
  mappend (CairoRender a) (CairoRender b) = CairoRender $ mappend <$> a <*> b
  mempty = return mempty

-- | Contains parameters that can exist /after/ the 'Gtk.Window' has been allocated. These
-- parameters are stored in an 'MVar' which is initialized in an empty state until the Gtk+ window
-- is actually made visible (when it recieves the first "configure" event). The reason for this is
-- that many of the 'Happlets.GUI.GUI' functions may try to evaluate a redraw before the window
-- exists and the DrawWIndow has been allocated.
data GtkWindowLive
  = GtkWindowLive
    { gtkDrawWindow  :: !Gtk.DrawWindow
    , theGtkPixmap   :: !Gtk.Pixmap
      -- ^ This is the 'Gtk.Pixmap' that contains the buffer of the image that is displayed in the
      -- Happlet window. Every time the window is resized, this object is re-allocated, so it is
      -- important that this value be accessible in only one location, do not store a copy of it
      -- into an MVar or IORef which other threads may write to arbitrarily, unless you like the
      -- image to be garbled.
    , theGtkGraphCtx :: !Gtk.GC
      -- ^ This is the old-fashioned GUI graphics context, which keeps track of things like
      -- foreground color, background color, clip region, and pen location. This is used for Gdk
      -- (not Gtk+) drawing primitives. As far as I know, this data structure is deprecated and does
      -- not even exist in Gtk+ version 3, but it is still the only way to perform certain
      -- graphics operations in Gtk+ version 2.
    }

data GtkWindowState
  = GtkWindowState
    { currentConfig         :: !Config
    , thisWindow            :: !(MVar GtkWindowState)
    , gtkWindowLive         :: !(MVar GtkWindowLive)
    , gtkWindow             :: !Gtk.Window
    , theInitReaction       :: !(ConnectReact PixSize)
    , theResizeReaction     :: !(ConnectReact PixCoord)
    , theVisibilityReaction :: !(ConnectReact Bool)
    , theFocusReaction      :: !(ConnectReact Bool)
    , theMouseHandler       :: !(ConnectReact Mouse)
    , theCursorHandler      :: !(ConnectReact Mouse)
    , theKeyHandler         :: !(ConnectReact Keyboard)
    , theAnimatorThread     :: !(ConnectReact AnimationMoment)
    }

data ConnectReact event
  = Disconnected
  | ConnectReact{ doDisconnect :: GtkState (), doReact :: event -> GtkState Bool }

evalConnectReact :: ConnectReact event -> event -> GtkState Bool
evalConnectReact = \ case
  Disconnected -> const $ return False
  ConnectReact{doReact=f} -> f

forceDisconnect :: Lens' GtkWindowState (ConnectReact event) -> GtkState ()
forceDisconnect connection = use connection >>= \ case
  ConnectReact{doDisconnect=discon} -> discon >> connection .= Disconnected
  Disconnected -> return ()

--gtkPixmap :: Lens' GtkWindowLive Gtk.Pixmap
--gtkPixmap = lens theGtkPixmap $ \ a b -> a{ theGtkPixmap = b }

--gtkGraphCtx :: Lens' GtkWindowLive Gtk.GC
--gtkGraphCtx = lens theGtkGraphCtx $ \ a b -> a{ theGtkGraphCtx = b }

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

--gtkCanvas :: GtkState Gtk.DrawWindow
--gtkCanvas = gets gtkWindowLive >>= liftIO . liftM gtkDrawWindow . readMVar

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
lockGtkWindow :: GtkWindow -> GtkState a -> IO a
lockGtkWindow win f = case win of
  GtkLockedWin{} -> error "lockGtkWindow: evaluated on an already locked 'GtkLockedWin' window."
  GtkUnlockedWin mvar -> modifyMVar mvar $ liftM (\ (a, b) -> (b, a)) . runGtkState f

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
  this <- newEmptyMVar
  live <- newEmptyMVar
  let env = GtkWindowState
        { currentConfig         = cfg
        , thisWindow            = this
        , gtkWindowLive         = live
        , gtkWindow             = window
        , theInitReaction       = Disconnected
        , theResizeReaction     = Disconnected
        , theVisibilityReaction = Disconnected
        , theFocusReaction      = Disconnected
        , theMouseHandler       = Disconnected
        , theCursorHandler      = Disconnected
        , theKeyHandler         = Disconnected
        , theAnimatorThread     = Disconnected
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
    logGUI $ "Gtk.widgetAddEvents env [Gtk.StructureMask]"
    Gtk.widgetAddEvents (gtkWindow env) [Gtk.StructureMask, Gtk.ExposureMask]
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
          liftIO $ withMVar (gtkWindowLive env) $ \ livest -> do
              Gtk.gcSetClipRegion (theGtkGraphCtx livest) region
              (w, h) <- Gtk.drawableGetSize (theGtkPixmap livest)
              logGUI $ "Gtk.drawRectangle 0 0 " ++ show w ++ ' ' : show h
              Gtk.drawRectangle canvas (theGtkGraphCtx livest) True 0 0 w h
              --liftIO $ gdkBlit canvas (theGtkGraphCtx env) (V2 0 0) region >> return True
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
      liftIO $ lockGtkWindow (GtkUnlockedWin $ thisWindow env) $ do
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
      liftIO $ lockGtkWindow (GtkUnlockedWin $ thisWindow env) $ do
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
      lockGtkWindow (GtkUnlockedWin $ thisWindow env) $ do
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
  liftIO $ modifyMVar_ (gtkWindowLive env) $ \ livest -> do
    oldDims <- Gtk.drawableGetSize (theGtkPixmap livest)
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
newGtkWindowLive cfg canvas size@(w, h) = do
  logGUI <- mkLogger "gtkAllocNewPixmap" True
  let depth = Just $ if isJust $ cfg ^. backgroundTransparency then 32 else 24
  let grey  = cfg ^. backgroundGreyValue
  logGUI $ "Gtk.pixmapNew " ++ show size ++ ' ' : show depth
  pixmap <- Gtk.pixmapNew (Just canvas) w h depth
  --logGUI $ "Gtk.gcNew -- should be Gtk.DrawWindow of 'window'"
  --graphCtx <- Gtk.gcNew canvas
  --gcVals   <- Gtk.gcGetValues graphCtx
  --logGUI $ "Gtk.gcSetValues"
  --Gtk.gcSetValues graphCtx gcVals
  --  { Gtk.background = dToGrey grey
  --  , Gtk.tile       = Just pixmap
  --  , Gtk.fill       = Gtk.Tiled
  --  }
  gc <- liftIO $ Gtk.gcNewWithValues canvas Gtk.newGCValues
    { Gtk.background = dToGrey grey
    , Gtk.tile       = Just pixmap
    , Gtk.fill       = Gtk.Tiled
    }
  return GtkWindowLive
    { gtkDrawWindow = canvas
    , theGtkPixmap = pixmap
    , theGtkGraphCtx = gc
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
  lockGtkWindow win $ do
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
    liftIO $ logGUI "Gtk.widgetShow" >> Gtk.widgetShow win

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
    void disable

-- Set which function redraws the window.
evalRedraw :: (PixSize -> CairoRender a) -> GtkState a
evalRedraw redraw = do
  logGUI <- mkLogger "evalRedraw" True
  env <- get
  liftIO $ withMVar (gtkWindowLive env) $ \ livest -> do
    -- Obtain the size from the draw window, but will not draw to the draw window.
    size <- fmap sampCoord . uncurry V2 <$> Gtk.drawableGetSize (gtkDrawWindow livest)
    logGUI "Gtk.renderWithDrawable -- to theGtkPixmap"
    a <- Gtk.renderWithDrawable (theGtkPixmap livest) $ runCairoRender $ redraw size
    (w, h) <- Gtk.drawableGetSize (gtkDrawWindow livest)
    region <- Gtk.regionRectangle $ Gtk.Rectangle 0 0 w h
    logGUI "Gtk.drawWindowInvalidateRegion"
    Gtk.drawWindowInvalidateRegion (gtkDrawWindow livest) region True
    return a

----------------------------------------------------------------------------------------------------

instance HappletWindow GtkWindow CairoRender where
  windowChangeHapplet = gtkSetHapplet
  onView = runGtkStateGUI . evalRedraw

instance Happlet2DGraphics CairoRender where
  clearScreen = unpackRGBA32Color >>> \ (r,g,b,a) -> CairoRender $ cairoClearCanvas r g b a
  drawLine = cairoDrawLine
  drawPath = cairoDrawPath
  drawRect = cairoDrawRect
  setPoint = error "TODO: canvasSetPoint has not yet been implemented in this Happlets back-end"
  getPoint = error "TODO: canvasGetPoint has not yet been implemented in this Happlets back-end"

-- | Push the graphics context by calling 'Cairo.save' before evaluating a given 'Cario.Render'
-- function. When evaluation completes, pop the graphics context by calling 'Cairo.restore'.
cairoPreserve :: CairoRender a -> CairoRender a
cairoPreserve f = CairoRender Cairo.save >> f <* CairoRender Cairo.restore

cairoMoveTo :: Point2D RealApprox -> CairoRender ()
cairoMoveTo = CairoRender . uncurry Cairo.moveTo . view pointXY . fmap unwrapRealApprox

cairoLineTo :: Point2D RealApprox -> CairoRender ()
cairoLineTo = CairoRender . uncurry Cairo.lineTo . view pointXY . fmap unwrapRealApprox

cairoDrawLine :: LineColor -> LineWidth -> Line2D RealApprox -> CairoRender ()
cairoDrawLine color width line = cairoPreserve $ do
  CairoRender $ do
    cairoSetColorRGBA32 color
    Cairo.setLineCap Cairo.LineCapRound
    Cairo.setLineWidth $ unwrapRealApprox width
  cairoMoveTo $ line ^. line2DHead
  cairoLineTo $ line ^. line2DTail
  CairoRender $ Cairo.stroke

cairoDrawPath :: LineColor -> LineWidth -> [Point2D RealApprox] -> CairoRender ()
cairoDrawPath color width =
  let run a ax = cairoPreserve $ do
        CairoRender $ do
          cairoSetColorRGBA32 color
          Cairo.setLineCap Cairo.LineCapRound
          Cairo.setLineWidth $ unwrapRealApprox width
          Cairo.setLineJoin Cairo.LineJoinMiter
        cairoMoveTo a
        forM_ ax cairoLineTo
        CairoRender Cairo.stroke
  in  \ case { [] -> return (); [a] -> run a [a]; a:ax -> run a ax; }

cairoDrawRect :: LineColor -> LineWidth -> FillColor -> Rect2D RealApprox -> CairoRender ()
cairoDrawRect lineColor width fillColor rect = cairoPreserve $ do
  CairoRender $ cairoSetColorRGBA32 fillColor
  cairoMoveTo $ rect ^. rect2DHead
  let ((x0,y0),(x1,y1)) = view pointXY *** view pointXY $
        (unwrapRealApprox <$> rect) ^. rect2DPoints
  CairoRender $ do
    Cairo.rectangle x0 y0 x1 y1
    Cairo.fill
    cairoSetColorRGBA32 lineColor
    Cairo.setLineWidth $ unwrapRealApprox width
    Cairo.setLineJoin Cairo.LineJoinMiter
    Cairo.rectangle x0 y0 x1 y1
    Cairo.stroke

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
  visibleEvents = installEventHandler "visibleEvents" visibilityReaction
    [Gtk.VisibilityNotifyMask] $ \ logGUI env next -> do
      let win = gtkWindow env
      logGUI "Glib.on Gtk.visibilityNotifyEvent"
      vis <- Glib.on win Gtk.visibilityNotifyEvent $
        Gtk.eventVisibilityState >>=
        liftIO . next . not . (== Gtk.VisibilityFullyObscured) >>
        return False
      return $ do
        Glib.signalDisconnect vis
  focusEvents   = installEventHandler "focusEvents" focusReaction
    [Gtk.FocusChangeMask] $ \ logGUI env next -> do
      let win = gtkWindow env
      logGUI "Glib.on Glib.focusInEvent"
      focin  <- Glib.on win Gtk.focusInEvent  $ liftIO (next True)  >> return False
      logGUI "Glib.on Glib.focusOutEvent"
      focout <- Glib.on win Gtk.focusOutEvent $ liftIO (next False) >> return False
      return $ do
        Glib.signalDisconnect focin
        Glib.signalDisconnect focout

instance CanResize GtkWindow where
  resizeEvents = installEventHandler "resizeEvents" resizeReaction [] (\ _ _ _ -> return $ pure ())

data AnimationThreadControl
  = AnimationThreadControl
    { animationThreadAlive :: !Bool
    , animationInitTime    :: !UTCTime
    }

instance CanAnimate GtkWindow where
  animationIsRunning = runGtkStateGUI $
    gets theAnimatorThread <&> \ case { Disconnected -> False; _ -> True; }
  stepFrameEvents react = do
    flip (installEventHandler "stepFrameEvents" animatorThread []) react $ \ logGUI env next -> do
      --let rate = env & theAnimationFrameRate . currentConfig -- TODO: restore this!!!
      let rate = 0.05 -- any slow rate
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
        logGUI "Glib.signalDisconnect"
        modifyIORef t0ref $ \ ctrl -> ctrl{ animationThreadAlive = False }
        Glib.timeoutRemove frame

instance CanKeyboard GtkWindow where
  keyboardEvents = installEventHandler "keyboardEvents" keyHandler
    [Gtk.KeyReleaseMask, Gtk.KeyPressMask] $ \ logGUI env next -> do
      let win = gtkWindow env
      logGUI "Glib.on Gtk.keyReleaseEvent"
      press   <- Glib.on win Gtk.keyReleaseEvent $
        handleKey False >>= liftIO . next >> return False
      logGUI "Glib.on Gtk.keyPressEvent"
      release <- Glib.on win Gtk.keyPressEvent $
        handleKey True  >>= liftIO . next >> return False
      return $ do
        logGUI "Glib.signalDisconnect"
        Glib.signalDisconnect press
        Glib.signalDisconnect release

instance CanMouse GtkWindow where
  providedMouseDevices = return []
  mouseEvents = \ case
    MouseButton -> installEventHandler "mouseButtonEvents" mouseHandler
      [Gtk.ButtonPressMask] $ \ logGUI env next -> do
        let win = gtkWindow env
        logGUI "Glib.on Gtk.buttonPressEvent"
        press <- Glib.on win Gtk.buttonPressEvent $
          handleMouse True >>= liftIO . next >> return False
        release <- Glib.on win Gtk.buttonReleaseEvent $
          handleMouse False >>= liftIO . next >> return False
        return $ do
          logGUI "Glib.signalDisconnect"
          Glib.signalDisconnect press
          Glib.signalDisconnect release
    MouseDrag   -> installEventHandler "mouseDragEvents" mouseHandler
      [Gtk.ButtonPressMask, Gtk.ButtonMotionMask] $ \ logGUI env next -> do
        let win = gtkWindow env
        logGUI "Glib.on Gtk.buttonPressEvent"
        press <- Glib.on win Gtk.buttonPressEvent $
          handleMouse True >>= liftIO . next >> return False
        logGUI "Glib.on Gtk.buttonReleaseEvent"
        release <- Glib.on win Gtk.buttonReleaseEvent $
          handleMouse False >>= liftIO . next >> return False
        logGUI "Glib.on Gtk.motionNotifyEvent"
        button <- Glib.on win Gtk.motionNotifyEvent $
          handleCursor True >>= liftIO . next >> return False
        return $ do
          logGUI "Glib.signalDisconnect"
          Glib.signalDisconnect press
          Glib.signalDisconnect release
          Glib.signalDisconnect button
    MouseAll    -> installEventHandler "mouseAllEvents" mouseHandler
      [Gtk.PointerMotionMask, Gtk.ButtonMotionMask] $ \ logGUI env next -> do
        let win = gtkWindow env
        logGUI "Glib.on Gtk.motionNotifyEvent"
        button  <- newIORef False
        press   <- Glib.on win Gtk.buttonPressEvent   $
          liftIO (writeIORef button False) >>
          handleMouse True  >>= liftIO . next >> return False
        release <- Glib.on win Gtk.buttonReleaseEvent $
          liftIO (writeIORef button True) >>
          handleMouse False >>= liftIO . next >> return False
        motion  <- Glib.on win Gtk.motionNotifyEvent  $
          liftIO (readIORef button) >>=
          handleCursor  >>=liftIO . next >> return False
        return $ do
          logGUI "Glib.signalDisconnect"
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
  -> [Gtk.EventMask]
  -> (LogGUI -> GtkWindowState -> (event -> IO Bool) -> IO (IO ()))
  -> (event -> GtkGUI model ())
  -> GtkGUI model ()
installEventHandler logString connectReact masks install react = do
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
          lockGtkWindow (GtkUnlockedWin $ thisWindow env) $ do
            stillAlive <- use connectReact >>= flip evalConnectReact event
            unless stillAlive $ forceDisconnect connectReact
            return stillAlive
        logGUI $ "Gtk.widgetAddEvents " ++ show masks
        Gtk.widgetAddEvents (gtkWindow env) masks
        return $ liftIO $ do
          logGUI $ "Gtk.widgetDelEvents " ++ show masks
          Gtk.widgetDelEvents (gtkWindow env) masks
          disconnect
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

----------------------------------------------------------------------------------------------------

-- | A 'GtkDrawing' is a function that produces a 'Diagrams.Core.Types.Diagram' from a
-- 'Diagrams.BoundingBox.BoundingBox'. Use the 'Diagrams.BoundingBox.BoundingBox' information to
-- inform the placement and scale of your diagram.
type GtkCairoDiagram = BoundingBox V2 Double -> Diagram Cairo

-- | This is a 'Happlets.View.Redraw' type function specific to this Gtk+ back-end provider.
type GtkRedraw a = CairoRender a

-- | This data type contains a pointer to an image buffer in memory, and also a function used to
-- perform some drawing to the pixel values.
data GtkImage
  = GtkImage
    { gtkPixmapMVar :: MVar Gtk.Pixmap
    }

instance CanBufferImages GtkWindow GtkImage CairoRender where
  newImageBuffer  (V2 (SampCoord w) (SampCoord h)) (CairoRender draw) = liftIO $ do
    pixmap <- Gtk.pixmapNew (Nothing :: Maybe Gtk.Pixmap)
      (fromIntegral w) (fromIntegral h) (Just 32)
    Gtk.renderWithDrawable pixmap draw
    mvar <- newMVar pixmap
    return GtkImage
      { gtkPixmapMVar = mvar
      }

  resizeImageBuffer (GtkImage{gtkPixmapMVar=mvar}) (V2 w h) (CairoRender draw) =
    liftIO $ modifyMVar mvar $ \ pixmap -> do
      pixmap <- Gtk.pixmapNew (Just pixmap) (fromIntegral w) (fromIntegral h) (Just 32)
      a <- Gtk.renderWithDrawable pixmap draw
      return (pixmap, a)

  drawImage (GtkImage{gtkPixmapMVar=mvar}) (CairoRender draw) =
    liftIO $ withMVar mvar $ flip Gtk.renderWithDrawable draw

  blitImage (GtkImage{gtkPixmapMVar=mvar}) offset = runGtkStateGUI $ do
    gtkwin <- gets gtkWindow
    canvas <- liftIO $ Gtk.widgetGetDrawWindow gtkwin
    liftIO $ withMVar mvar $ \ pixmap -> gdkBlit canvas pixmap offset

  blitImageTo (GtkImage{gtkPixmapMVar=src}) (GtkImage{gtkPixmapMVar=targ}) offset =
    liftIO $ withMVar src $ \ src -> withMVar targ $ \ targ -> gdkBlit targ src offset

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

-- | Use the Happlets-native color data type 'Happlets.Draw.Color.PackedRGBA32' to set the Cairo
-- "source" color in the Cairo context.
cairoSetColorRGBA32 :: PackedRGBA32 -> Cairo.Render ()
cairoSetColorRGBA32 = unpackRGBA32Color >>> \ (r,g,b,a) -> Cairo.setSourceRGBA r g b a

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
  , doWindowDelete          = flip lockGtkWindow $ get >>= liftIO . deleteWin
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
    modifyMVar_ gtkInitCheck $ return . const True

-- | Launch the Gtk+ main event loop. This function will never return. If you fail to call this
-- function, your GUI will never run.
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
