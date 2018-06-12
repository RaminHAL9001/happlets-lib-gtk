module Happlets.Lib.Gtk.TestSuite where

import           Happlets.Lib.Gtk

import           Control.Arrow

import           Linear.V2

import qualified Graphics.Rendering.Cairo as Cairo

----------------------------------------------------------------------------------------------------

-- This data structure contains the GUI functions which initialize each Happlet that can be attached
-- to the test suite window.
data TestSuite
  = TestSuite
    { switchToPulseCircle :: GtkGUI RedGrid ()
    , switchToRedGrid     :: GtkGUI PulseCircle ()
    }

main :: IO ()
main = happlet gtkHapplet $ do
  registeredAppName   .= "Happlets-Test"
  windowTitleBar      .= "Happlets Test"
  recommendWindowSize .= (640, 480)
  quitOnWindowClose   .= True
  
  win         <- newWindow
  pulsecircle <- newHapplet PulseCircle
    { thePulseCircleRadius     = 20
    , thePulseCirclePosition   = V2   0   0
    , thePulseCircleWindowSize = V2 640 480
    }

  redgrid     <- newHapplet (RedGrid 64.0)

  let testSuite = TestSuite
        { switchToPulseCircle = windowChangeHapplet pulsecircle $ pulseCircleGUI testSuite
        , switchToRedGrid     = windowChangeHapplet redgrid     $ redGridGUI     testSuite
        }

  attachWindow True win redgrid $ redGridGUI testSuite

----------------------------------------------------------------------------------------------------

newtype RedGrid = RedGrid { theRedGridScale :: RealApprox }
  deriving (Eq, Ord, Show, Read)

redGridScale :: Lens' RedGrid RealApprox
redGridScale = lens theRedGridScale $ \ a b -> a{ theRedGridScale = b }

redGridDraw :: RealApprox -> PixSize -> CairoRender ()
redGridDraw scale = fmap realToFrac >>> \ size@(V2 w h) ->
  if scale <= 1 then clearScreen red else do
    let (V2 centerX centerY) = (/ 2.0) <$> size
    let around top center =
          (takeWhile (>=  0) $ [1 ..] >>= \ i -> [center - scale * i :: RealApprox]) ++
          (takeWhile (< top) $ [0 ..] >>= \ i -> [center + scale * i :: RealApprox])
    let mkLine v2 top i = Line2D (Point2D $ v2 i 0) (Point2D $ v2 i top)
    clearScreen (black & alphaChannel .~ 0.9)
    forM_ (around w centerX) $ drawLine red 1.0 . mkLine V2 h
    forM_ (around h centerY) $ drawLine red 1.0 . mkLine (flip V2) w

redGridGUI :: TestSuite -> PixSize -> GtkGUI RedGrid ()
redGridGUI ctx _size = do
  let draw = use redGridScale >>= onView . redGridDraw
  resizeEvents $ const draw
  mouseEvents MouseButton $ \ (Mouse _ down _ button _) -> when down $ case button of
    RightClick -> switchToPulseCircle ctx
    _          -> do
      scale <- use redGridScale
      redGridScale .= if scale <= 4.0 then 64.0 else scale / 2.0
      draw
  draw

----------------------------------------------------------------------------------------------------

data PulseCircle
  = PulseCircle
    { thePulseCircleRadius     :: !Double
    , thePulseCirclePosition   :: !(V2 Double)
    , thePulseCircleWindowSize :: !PixSize
    }
  deriving (Eq, Ord, Show, Read)

pulseCircleRadius :: Lens' PulseCircle Double
pulseCircleRadius = lens thePulseCircleRadius $ \ a b -> a{ thePulseCircleRadius = b }

pulseCirclePosition :: Lens' PulseCircle (V2 Double)
pulseCirclePosition = lens thePulseCirclePosition $ \ a b -> a{ thePulseCirclePosition = b }

pulseCircleWindowSize :: Lens' PulseCircle PixSize
pulseCircleWindowSize = lens thePulseCircleWindowSize $ \ a b -> a{ thePulseCircleWindowSize = b }

type Radius  = Double
type PenSize = Double
type Red     = Double
type Green   = Double
type Blue    = Double
type Alpha   = Double

circle :: V2 Double -> Radius -> Red -> Green -> Blue -> Alpha -> CairoRender ()
circle (V2 x y) radius red green blue alpha = CairoRender $ do
  op <- Cairo.getOperator
  Cairo.setOperator Cairo.OperatorSource
  Cairo.setSourceRGBA red green blue alpha
  Cairo.arc (x + 0.5) (y + 0.5) radius 0.0 (2.0 * pi)
  Cairo.fill
  Cairo.setOperator op

pulseCircleGUI :: TestSuite -> PixSize -> GtkGUI PulseCircle ()
pulseCircleGUI ctx initSize@(V2 (SampCoord w) (SampCoord h)) = do
  -- Initialize the model such that the circle is placed in the middle of the window.
  pulseCirclePosition .= V2 (realToFrac w / 2) (realToFrac h / 2)
  pulseCircleWindowSize .= initSize

  -- Declare a function for drawing the model:
  let drawDot clear
        (PulseCircle{thePulseCircleRadius=oldR,thePulseCirclePosition=oldXY})
        (PulseCircle{thePulseCircleRadius=newR,thePulseCirclePosition=newXY}) = \ _size -> do
            when clear $ clearScreen (black & alphaChannel .~ 0.9)
            circle oldXY (max oldR newR + 1.0) 0.0  0.0  0.0  0.9
            circle newXY           newR        0.0  0.0  1.0  1.0

  -- Install the mouse event handling function.
  mouseEvents MouseDrag $ \ (Mouse _ down _ button newXY) -> when down $ case button of
    RightClick -> switchToRedGrid ctx
    _          -> do
      old <- get
      pulseCirclePosition .= (realToFrac <$> newXY)
      new <- get
      onView $ drawDot False old new

  -- On resize, simply redraw the window without modifying the model. We don't need to use the new
  -- size information for the window.
  resizeEvents $ \ newSize@(V2 newW newH) -> do
    old <- get
    let (V2 oldW oldH) = old ^. pulseCircleWindowSize
    let (V2 x    y   ) = old ^. pulseCirclePosition
    let f n new old = n * realToFrac new / realToFrac old
    pulseCirclePosition   .= V2 (f x newW oldW) (f y newH oldH)
    pulseCircleWindowSize .= newSize
    new <- get
    onView $ drawDot True old new

  -- Install an animator thread which makes the radius of the circle as a function of time.
  stepFrameEvents $ \ t -> do
    old <- get
    pulseCircleRadius .= 20 * sin (2*pi * realToFrac t) + 40
    new <- get
    onView $ drawDot False old new

  -- Clear the window. If you don't do this, the behavior of what the initial window will look
  -- like is not defined in the Happlets protocol. It could be a default solid color, or it could
  -- be random garbage.
  model <- get
  onView $ drawDot True model model
