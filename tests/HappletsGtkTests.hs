module Main where

import           Happlets.Provider.Gtk2

import           Control.Concurrent

import           Data.Maybe
import qualified Data.Text as Strict

import qualified Graphics.Rendering.Cairo as Cairo

----------------------------------------------------------------------------------------------------

-- This data structure contains the GUI functions which initialize each Happlet that can be attached
-- to the test suite window.
data TestSuite
  = TestSuite
    { testSuiteSharedState :: MVar Strict.Text
    , switchToPulseCircle  :: GtkGUI RedGrid ()
    , switchToRedGrid      :: GtkGUI PulseCircle ()
    }

main :: IO ()
main = happlet gtkHapplet $ do
  registeredAppName   .= "Happlets-Test"
  initWindowTitleBar  .= "Happlets Test"
  recommendWindowSize .= (640, 480)
  quitOnWindowClose   .= True

  mvar        <- liftIO $ newMVar "Main"
  thisThread  <- liftIO myThreadId
  pulsecircle <- newHapplet PulseCircle
    { thePulseCircleRadius     = 20
    , thePulseCirclePosition   = V2 (-1) (-1)
    , thePulseCircleWindowSize = V2  640  480
    , thePulseCircleColor      = blue
    , thePulseCircleClockStep  = 0
    , thePulseCircleWorker     = thisThread
    }

  redgrid     <- newHapplet (RedGrid 64.0 Nothing Nothing)

  let testSuite = TestSuite
        { testSuiteSharedState = mvar
        , switchToPulseCircle  = changeRootHapplet pulsecircle $ pulseCircleGUI testSuite
        , switchToRedGrid      = changeRootHapplet redgrid     $ redGridGUI     testSuite
        }

  attachWindow True redgrid $ redGridGUI testSuite

----------------------------------------------------------------------------------------------------

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
redGridDraw scale winsize = do
  let (V2 w h) = realToFrac <$> winsize
  if scale <= 1 then clearScreen red else do
    let mkLine v2 top i = Draw2DLine $ line2D & (line2DTail .~ v2 i 0) & (line2DHead .~ v2 i top)
    clearScreen (black & alphaChannel .~ 0.9)
    strokeWeight .= (1.0 :: Double)
    strokeColor  .= PaintSolidColor red
    forM_ [0::Int .. floor (w / scale)] $ \ i -> do
      shape .= mkLine V2 h (0.5 + scale * realToFrac i)
      stroke
    forM_ [0::Int .. floor (h / scale)] $ \ i -> do
      shape .= mkLine (flip V2) w (0.5 + scale * realToFrac i)
      stroke
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
  resizeEvents CanvasResizeCopy $ \ _oldsize newsize -> cancelIfBusy >> draw newsize
  mouseEvents MouseAll $ \ (Mouse _ down _ button pt1@(V2 x1 y1)) -> do
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

----------------------------------------------------------------------------------------------------

data PulseCircle
  = PulseCircle
    { thePulseCircleRadius     :: !Double
    , thePulseCirclePosition   :: !(V2 Double)
    , thePulseCircleColor      :: !Color
    , thePulseCircleWindowSize :: !PixSize
    , thePulseCircleClockStep  :: !Int
    , thePulseCircleWorker     :: !ThreadId
    }
  deriving (Eq)

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
pulseCircleGUI ctx initSize@(V2 (SampCoord w) (SampCoord h)) = do
  -- Initialize the model such that the circle is placed in the middle of the window.
  pulseCirclePosition %= \ old@(V2 oldW oldH) ->
    if oldW < 0 || oldH < 0 then V2 (realToFrac w / 2) (realToFrac h / 2) else old
  pulseCircleWindowSize .= initSize

  let loop runGUI = do
        putStrLn "Draw white line..."
        result <- runGUI $ do
          (V2 (SampCoord w) (SampCoord h)) <- use pulseCircleWindowSize
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
        print result
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
  mouseEvents MouseDrag $ \ (Mouse _ down _ button newXY) -> when down $ case button of
    RightClick -> switchToRedGrid ctx
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
    pulseCircleRadius .= 20 * sin (2*pi * realToFrac t) + 40
    new <- get
    onCanvas $ drawDot False old new

  -- Clear the window. If you don't do this, the behavior of what the initial window will look
  -- like is not defined in the Happlets protocol. It could be a default solid color, or it could
  -- be random garbage.
  model <- get
  onCanvas $ drawDot True model model
