module Main where

import           Happlets.Provider.Gtk2
import           Happlets.Scene
import           Happlets.View.Color (red)

import           Control.Category ((>>>))
import           Control.Concurrent

import           Data.Maybe
import qualified Data.Text as Strict
import           Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
--import qualified Data.Vector.Mutable as MVec

--import           Linear.V2

import qualified Graphics.Rendering.Cairo as Cairo

import Debug.Trace

-- =================================================================================================

-- This data structure contains the GUI functions which initialize each Happlet that can be attached
-- to the test suite window.
data TestSuite
  = TestSuite
    { testSuiteSharedState :: MVar Strict.Text
    , switchToPulseCircle  :: GtkGUI RedGrid ()
    , switchToRedGrid      :: GtkGUI PulseCircle ()
    , switchToSceneTest    :: GtkGUI PulseCircle ()
    }

main :: IO ()
main = happlet gtkHapplet $ do
  registeredAppName   .= "Happlets-Test"
  initWindowTitleBar  .= "Happlets Test"
  recommendWindowSize .= (640, 480)
  quitOnWindowClose   .= True
  setMaxLogReportLevel DEBUG

  mvar        <- liftIO $ newMVar "Main"
  thisThread  <- liftIO myThreadId
  t0          <- liftIO getCurrentTime
  pulsecircle <- newHapplet PulseCircle
    { thePulseCircleStartTime  = t0
    , thePulseCircleRadius     = 20
    , thePulseCirclePosition   = V2 (-1) (-1)
    , thePulseCircleWindowSize = V2  640  480
    , thePulseCircleColor      = blue
    , thePulseCircleClockStep  = 0
    , thePulseCircleWorker     = thisThread
    }

  redgrid     <- newHapplet (RedGrid 64.0 Nothing Nothing)

  scenetest   <- newActHapplet circleGroupInit

  let testSuite = TestSuite
        { testSuiteSharedState = mvar
        , switchToPulseCircle  = changeRootHapplet pulsecircle $ pulseCircleGUI testSuite
        , switchToRedGrid      = changeRootHapplet redgrid     $ redGridGUI     testSuite
        , switchToSceneTest    = changeRootHapplet scenetest   $ circleGroupGUI
        }

  attachWindow True redgrid $ redGridGUI testSuite

-- =================================================================================================

data RedGrid
  = RedGrid
    { theRedGridScale :: Double
    , theLastMouse    :: Maybe PixCoord
    , theMouseBox     :: Maybe TextBoundingBox
    }
  deriving Eq

lastMouse :: Lens' RedGrid (Maybe PixCoord)
lastMouse = lens theLastMouse $ \ a b -> a{ theLastMouse = b }

redGridScale :: Lens' RedGrid Double
redGridScale = lens theRedGridScale $ \ a b -> a{ theRedGridScale = b }

mouseBox :: Lens' RedGrid (Maybe TextBoundingBox)
mouseBox = lens theMouseBox $ \ a b -> a{ theMouseBox = b }

redGridDraw :: Double -> PixSize -> CairoRender ()
redGridDraw scale winsize@(V2 w h) =
  if scale <= 1 then clearScreen red else do
    let mkLine v2 top i = line2D &
          (line2DTail .~ v2 i 0) &
          (line2DHead .~ v2 i top)
    clearScreen (black & alphaChannel .~ 0.9)
    draw2D mempty $ drawing
      [ Draw2DLines 1
        (paintColor red) $
        ( (\ i -> mkLine V2 h $ floor $ scale * realToFrac i) <$>
          [0::Int .. floor (realToFrac w / scale)]
        ) ++
        ( (\ i -> mkLine (flip V2) w $ floor $ scale * realToFrac i) <$>
          [0::Int .. floor (realToFrac h / scale)]
        )
      ]
    void $ screenPrinter $
      withFontStyle (do{ fontForeColor .= white; fontSize .= 16.0; }) $ do
        renderOffset .= V2 0 0
        gridRow      .= 0
        gridColumn   .= 0
        displayString $ "Grid size = " ++ show scale ++ "\n\nWindow = " ++ show winsize ++ "\n"

redGridGUI :: TestSuite -> PixSize -> GtkGUI RedGrid ()
redGridGUI ctx _size = do
  let mvar = testSuiteSharedState ctx
  let draw size = use redGridScale >>= onCanvas . flip redGridDraw size
  changeEvents $ liftIO $ do
    putStrLn "change away from Red Grid"
    void $ swapMVar mvar "Red Grid"
  resizeEvents CanvasResizeCopy $ \ _oldsize newsize -> draw newsize
  mouseSignals MouseAll $ \ (MouseSignal _ down _ button pt1@(V2 x1 y1)) -> do
    use lastMouse >>= \ case
      Nothing         -> return ()
      Just (V2 x0 y0) -> refreshRegion
        [rect2D & rect2DTail .~ V2 (x0 - 22) (y0 - 22) & rect2DHead .~ V2 (x0 + 22) (y0 + 22)]
    if down
     then do
      case button of
        RightClick -> switchToPulseCircle ctx
        LeftClick  -> do
          redGridScale %= \ scale -> if scale <= 4.0 then 64.0 else scale / 2.0
          getConfig windowSize >>= draw . rect2DSize
        _          -> return ()
      scale <- use redGridScale
      mb <- use mouseBox
      refreshRegion $ fmap ((sampCoord :: Int -> SampCoord) . floor) <$> maybeToList mb
      mb <- onOSBuffer $ screenPrinter $
        withFontStyle (do{ fontForeColor .= white; fontSize .= scale; }) $ do
          gridRow      .= (-1)
          gridColumn   .= (-10)
          renderOffset .= (realToFrac <$> pt1)
          displayString $ show pt1
      mouseBox .= mb
     else use mouseBox >>= \ case
      Nothing  -> return ()
      Just box -> do
        refreshRegion [(sampCoord :: Int -> SampCoord) . floor <$> box]
        mouseBox .= Nothing
    onOSBuffer $ cairoRender $ do
      Cairo.setLineWidth 2.0
      cairoSetColor chartreuse
      Cairo.arc (realToFrac x1 + 0.5) (realToFrac y1 + 0.5) 20.0 0 (2*pi)
      Cairo.stroke
    lastMouse .= Just pt1
  getConfig windowSize >>= draw . rect2DSize

-- =================================================================================================

data PulseCircle
  = PulseCircle
    { thePulseCircleStartTime  :: !UTCTime
    , thePulseCircleRadius     :: !Double
    , thePulseCirclePosition   :: !(V2 Double)
    , thePulseCircleColor      :: !Color
    , thePulseCircleWindowSize :: !PixSize
    , thePulseCircleClockStep  :: !Int
    , thePulseCircleWorker     :: !ThreadId
    }
  deriving (Eq)

pulseCircleStartTime :: Lens' PulseCircle UTCTime
pulseCircleStartTime = lens thePulseCircleStartTime $ \ a b -> a{ thePulseCircleStartTime = b }

pulseCircleRadius :: Lens' PulseCircle Double
pulseCircleRadius = lens thePulseCircleRadius $ \ a b -> a{ thePulseCircleRadius = b }

pulseCirclePosition :: Lens' PulseCircle (V2 Double)
pulseCirclePosition = lens thePulseCirclePosition $ \ a b -> a{ thePulseCirclePosition = b }

pulseCircleColor :: Lens' PulseCircle Color
pulseCircleColor = lens thePulseCircleColor $ \ a b -> a{ thePulseCircleColor = b }

pulseCircleWindowSize :: Lens' PulseCircle PixSize
pulseCircleWindowSize = lens thePulseCircleWindowSize $ \ a b -> a{ thePulseCircleWindowSize = b }

pulseCircleWorker :: Lens' PulseCircle ThreadId
pulseCircleWorker = lens thePulseCircleWorker $ \ a b -> a{ thePulseCircleWorker = b }

pulseCircleClockStep :: Lens' PulseCircle Int
pulseCircleClockStep = lens thePulseCircleClockStep $ \ a b -> a{ thePulseCircleClockStep = b }

type Radius  = Double
type PenSize = Double

circle :: V2 Double -> Radius -> Color -> CairoRender ()
circle (V2 x y) radius color = cairoRender $ do
  op <- Cairo.getOperator <* Cairo.setOperator Cairo.OperatorSource
  cairoSetColor color
  Cairo.arc (x + 0.5) (y + 0.5) radius 0.0 (2.0 * pi)
  Cairo.fill
  Cairo.setOperator op

pulseCircleGUI :: TestSuite -> PixSize -> GtkGUI PulseCircle ()
pulseCircleGUI ctx initSize@(V2 w h) = do
  -- Initialize the model such that the circle is placed in the middle of the window.
  pulseCirclePosition %= \ old@(V2 oldW oldH) ->
    if oldW < 0 || oldH < 0 then V2 (realToFrac w / 2) (realToFrac h / 2) else old
  pulseCircleWindowSize .= initSize

  let loop runGUI = do
        putStrLn "Draw white line..."
        result <- runGUI $ do
          (V2 w h) <- use pulseCircleWindowSize
          let (x, y) = (realToFrac $ div w 2, realToFrac $ div h 2)
          let r = min x y
          t <- use pulseCircleClockStep
          pulseCircleClockStep %= (\ t -> mod (t + 1) 60)
          let theta = negate $ 2.0 * pi * realToFrac (mod t 60) / 60.0 - pi / 2.0
          onCanvas $ cairoRender $ do
            Cairo.moveTo x y
            Cairo.lineTo (x + r * cos theta) (y - r * sin theta)
            cairoSetColor white
            Cairo.stroke
        putStrLn $ case result of
          ActionOK   a   -> "OK: " ++ show a
          ActionHalt     -> "HALT"
          ActionCancel   -> "CANCEL"
          ActionFail msg -> "FAIL: " ++ Strict.unpack msg
        threadDelay 1000000
        loop runGUI

  forkGUI loop >>= assign pulseCircleWorker

  let mvar = testSuiteSharedState ctx
  -- Declare a function for drawing the model:
  let drawDot clear
        (PulseCircle{thePulseCircleRadius=oldR,thePulseCirclePosition=oldXY})
        (PulseCircle
         {thePulseCircleRadius=newR
         ,thePulseCirclePosition=newXY
         ,thePulseCircleColor=color
         }) = do
            let bg = black & alphaChannel .~ 0.9
            when clear $ clearScreen bg
            circle oldXY (max oldR newR + 1.0) bg
            circle newXY           newR        color
            --cairoRender $ do
            --  cameFrom <- liftIO $ readMVar mvar
            --  Cairo.moveTo  5.0  10.0
            --  Cairo.setSourceRGBA  1.0  1.0  1.0  1.0
            --  Cairo.showText $ "came from " <> cameFrom

  changeEvents $ do
    otherThread <- use pulseCircleWorker
    liftIO $ do
      putStrLn "Began changing away from Pulse Circle."
      killThread otherThread
      void $ swapMVar mvar "Pulse Circle"
      putStrLn "Completed changing away from Pulse Circle."

  keyboardEvents $ \ case
    (Keyboard True (ModifierBits 0) (CharKey ' ')) -> do
      pulseCircleColor %= \ case
        c | c == blue    -> red
        c | c == red     -> lime
        c | c == lime    -> yellow
        c | c == yellow  -> cyan
        c | c == cyan    -> green
        c | c == green   -> magenta
        c | c == magenta -> white
        _                -> blue
    _ -> return ()

  -- Install the mouse event handling function.
  mouseSignals MouseDrag $ \ (MouseSignal _ down _ button newXY) -> when down $ case button of
    RightClick -> switchToSceneTest ctx
    _          -> do
      old <- get
      pulseCirclePosition .= (realToFrac <$> newXY)
      new <- get
      onCanvas $ drawDot False old new

  -- On resize, simply redraw the window without modifying the model. We don't need to use the new
  -- size information for the window.
  resizeEvents CanvasResizeClear $ \ _oldsize newSize@(V2 newW newH) -> do
    old <- get
    let (V2 oldW oldH) = old ^. pulseCircleWindowSize
    let (V2 x    y   ) = old ^. pulseCirclePosition
    let f n new old = n * realToFrac new / realToFrac old
    pulseCirclePosition   .= V2 (f x newW oldW) (f y newH oldH)
    pulseCircleWindowSize .= newSize
    new <- get
    onCanvas $ drawDot True old new

  -- Install an animator thread which makes the radius of the circle as a function of time.
  stepFrameEvents $ \ t -> do
    old <- get
    let dt = diffUTCTime t $ thePulseCircleStartTime old
    pulseCircleRadius .= 20 * sin (2*pi * realToFrac dt) + 40
    new <- get
    onCanvas $ drawDot False old new

  -- Clear the window. If you don't do this, the behavior of what the initial window will look
  -- like is not defined in the Happlets protocol. It could be a default solid color, or it could
  -- be random garbage.
  model <- get
  onCanvas $ drawDot True model model

-- =================================================================================================

data GridAxis
  = GridAxis
    { theAxisColor   :: !Color
    , theAxisOffset  :: !Double
    , theAxisScale   :: !Double
    , theAxisSpacing :: !Double
    }

data GridOrientation = Horizontal | Vertical
  deriving (Eq, Ord, Enum, Bounded)

axisColor :: Lens' GridAxis Color
axisColor = lens theAxisColor $ \ a b -> a{ theAxisColor = b }

axisOffset :: Lens' GridAxis Double
axisOffset = lens theAxisOffset $ \ a b -> a{ theAxisOffset = b }

axisScale :: Lens' GridAxis Double
axisScale = lens theAxisScale $ \ a b -> a{ theAxisScale = b }

axisSpacing :: Lens' GridAxis Double
axisSpacing = lens theAxisSpacing $ \ a b -> a{ theAxisSpacing = b }

background :: Color -> Script any (TypedActor Color)
background = actor $ do
  color <- get
  size <- getViewSize
  onDraw $ drawing
    [ to2DShape
      (FillOnly $ paintColor color)
      [rect2D & rect2DHead .~ size]
    ]

gridAxis :: GridOrientation -> GridAxis -> Script any (TypedActor GridAxis)
gridAxis _orient = actor $ do
  _isize@(V2 w h) <- getViewSize
  onDraw $ drawing
    [ Draw2DLines 1
      (PaintSource BlitSource $ SolidColorSource red)
      [ line2D &
        line2DTail .~ V2 (div w 2) 0 &
        line2DHead .~ V2 (div w 2) h
      , line2D &
        line2DTail .~ V2 0 (div h 2) &
        line2DHead .~ V2 w (div h 2)
      ]
    ]

-- =================================================================================================

data MobileCircleN n
  = MobileCircle
    { theMobCircUniqId   :: !Int
    , theMobCircOrigin   :: !(V2 n)
    , theMobCircColorSym :: !ColorSym
    }

type MobileCircle = MobileCircleN SampCoord

instance Has2DOrigin MobileCircleN where
  origin2D = lens theMobCircOrigin $ \ a b -> a{ theMobCircOrigin = b }

newCircleGroup :: CircleGroup
newCircleGroup = CircleGroup 0

type ColorSym = Int

mobCircRadius :: SampCoord
mobCircRadius = 20

mobCircColorSym :: Lens' (MobileCircleN n) ColorSym
mobCircColorSym = lens theMobCircColorSym $ \ a b -> a{ theMobCircColorSym = b }

mobCircUniqId :: Lens' (MobileCircleN n) Int
mobCircUniqId = lens theMobCircUniqId $ \ a b -> a{ theMobCircUniqId = b }

mobCircInBounds :: PixelMouse -> Script MobileCircle ()
mobCircInBounds (Mouse2D evt _) = do
  o <- use origin2D
  let (V2 x y) = evt - o
  let r = mobCircRadius
  guard $ x*x + y*y <= r*r

mobCircIntToColor :: Int -> Color
mobCircIntToColor = (`mod` 11) >>> \ case
  1  -> magenta
  2  -> red
  3  -> orange
  4  -> yellow
  5  -> lime
  6  -> cyan
  7  -> blue
  8  -> purple
  9  -> olive
  10 -> grey
  _  -> white

mobCircRotateColor :: Script MobileCircle ()
mobCircRotateColor = modifying mobCircColorSym $ (`mod` 11) . (+ 1) 

mobCircInit :: Script MobileCircle ()
mobCircInit = do
  uniqId <- gets theMobCircUniqId
  o <- gets theMobCircOrigin
  color <- gets $ mobCircIntToColor . theMobCircColorSym
  let label = Strict.pack $ show uniqId <> " " <> show color <> " " <> show o
  selfLabel $ const label
  report DEBUG $ "exec: mobCircInit (" <> label <> ")"
  onDraw $ trace ("MobileCircle onDraw handler: color=" <> show color) $ drawing
    [ Draw2DShapes
      (StrokeOnly 4 (paintColor color))
      [Draw2DArc $ origin2D .~ o $ arc2D]
    ]
  onMouseDown RightMouseButton $ const $ EventAction
    { theActionText = "MobileCircle " <> Strict.pack (show uniqId) <> " grabFocus"
    , theAction =
      trace "MobileCircle onClick handler" $
      mobCircInBounds >=> const (trace "MobileCircle onClick -> grabFocus" grabFocus)
    }

newtype CircleGroup = CircleGroup Int

circleGroupDesktop :: Script CircleGroup ()
circleGroupDesktop = do
  selfLabel $ const "CircleGroup"
  report DEBUG "exec: circleGroupDesktop"
  put $ CircleGroup 0
  onDraw $ drawing [Draw2DReset]
  onMouseDown RightMouseButton $ const $ EventAction
    { theActionText = "\"the circle group\""
    , theAction = \ (Mouse2D location _) -> do
        report DEBUG "CircleGroup.onRightClick action triggered"
        (CircleGroup count) <- get
        when (count < 16) $ do
          modify (\ (CircleGroup i) -> CircleGroup (i + 1))
          actor mobCircInit MobileCircle
            { theMobCircUniqId = count
            , theMobCircColorSym = count
            , theMobCircOrigin = location
            }
          pure ()
    }
  stats <- getEventHandlerStats
  report DEBUG $ "after circleGroupDesktop:\n" <> Strict.pack (show stats)

circleGroupInit :: Script Scene ()
circleGroupInit = do
  report DEBUG "exec: circleGroupInit"
  actor circleGroupDesktop newCircleGroup >>= onStage
  stats <- getEventHandlerStats
  report DEBUG $ "after circleGroupInit:\n" <> Strict.pack (show stats)

circleGroupGUI :: PixSize -> GtkGUI Act ()
circleGroupGUI size = do
  report DEBUG "exec: changeRootHapplet circleGroupGUI"
  actWindow size
