module Happlets.Provider.Cairo where

import           Happlets
import           Happlets.Provider.Gtk2.Debug

import           Control.Arrow
import           Control.Concurrent

import           Data.Array.MArray
import           Data.Bits
import qualified Data.Text           as Strict
import qualified Data.Vector.Unboxed as UVec
import           Data.Word

import qualified Graphics.Rendering.Cairo           as Cairo
import qualified Graphics.Rendering.Cairo.Matrix    as Cairo

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
mkLogger func = return $ \ sel msg -> if sel .&. debugThisModule == mempty then return () else do
  tid <- myThreadId
  traceM $ '[':show tid++"][Happlets.Lib.Gtk."++func++']':msg
{-# INLINE mkLogger #-}

----------------------------------------------------------------------------------------------------

-- | This data type contains a pointer to an image buffer in memory, and also a function used to
-- perform some drawing to the pixel values.
newtype CairoPixelBuffer = CairoPixelBuffer{ gtkCairoSurfaceMVar :: MVar CairoRenderState }

----------------------------------------------------------------------------------------------------

-- | A monadic wrapper around a 'Cario.Render' monad. The only reason for this to exist is because
-- there needs to be an instance of 'Data.Semigroup.Semigroup'.
--
-- The 'Controller' type is defined to use the 'CairoRender' as it's @view@ type. You can also
-- convert a 'GtkCairoDiagram' to a 'CairoRender' using the 'gtkCairoDiagram' function.
newtype CairoRender a = CairoRender (StateT CairoRenderState Cairo.Render a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState CairoRenderState)

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

data CairoGeometry n
  = CairoGeometry
    { theCairoShape         :: !(Draw2DShape n)
    , theCairoLineWidth     :: !(LineWidth n)
    , theCairoBlitTransform :: !(Transform2D n)
    }

instance Map2DShape CairoGeometry where
  map2DShape f geom = CairoGeometry
    { theCairoShape         = map2DShape f $ theCairoShape geom
    , theCairoLineWidth     = f $ theCairoLineWidth geom
    , theCairoBlitTransform = fmap (fmap f) $ theCairoBlitTransform geom
    }

cairoShape :: Lens' (CairoGeometry n) (Draw2DShape n)
cairoShape = lens theCairoShape $ \ a b -> a{ theCairoShape = b }

cairoLineWidth :: Lens' (CairoGeometry n) (LineWidth n)
cairoLineWidth = lens theCairoLineWidth $ \ a b -> a{ theCairoLineWidth = b }

cairoBlitTransform :: Lens' (CairoGeometry n) (Transform2D n)
cairoBlitTransform = lens theCairoBlitTransform $ \ a b -> a{ theCairoBlitTransform = b }

data CairoRenderState
  = CairoRenderState
    { theCairoKeepWinSize        :: !PixSize
    , theCanvasResizeMode        :: !CanvasResizeMode
    , theCanvasFillColor         :: !PaintSource
    , theCanvasStrokeColor       :: !PaintSource
    , theCairoGeometry           :: !(CairoGeometry Double)
    , theCairoClipRect           :: !(Rect2D SampCoord)
    , theCairoRenderMode         :: !CairoRenderMode
    , theCairoScreenPrinterState :: !ScreenPrinterState
    , theGtkCairoSurface         :: !(Maybe Cairo.Surface)
    }

cairoKeepWinSize :: Lens' CairoRenderState PixSize
cairoKeepWinSize = lens theCairoKeepWinSize $ \ a b -> a{ theCairoKeepWinSize = b }

canvasFillColor :: Lens' CairoRenderState PaintSource
canvasFillColor = lens theCanvasFillColor $ \ a b -> a{ theCanvasFillColor = b }

canvasStrokeColor :: Lens' CairoRenderState PaintSource
canvasStrokeColor = lens theCanvasStrokeColor $ \ a b -> a{ theCanvasStrokeColor = b }

cairoGeometry :: Lens' CairoRenderState (CairoGeometry Double)
cairoGeometry = lens theCairoGeometry $ \ a b -> a{ theCairoGeometry = b }

cairoClipRect :: Lens' CairoRenderState (Rect2D SampCoord)
cairoClipRect = lens theCairoClipRect $ \ a b -> a{ theCairoClipRect = b }

cairoRenderMode :: Lens' CairoRenderState CairoRenderMode
cairoRenderMode = lens theCairoRenderMode $ \ a b -> a{ theCairoRenderMode = b }

gtkCairoSurface :: Lens' CairoRenderState (Maybe Cairo.Surface)
gtkCairoSurface = lens theGtkCairoSurface $ \ a b -> a{ theGtkCairoSurface = b }

canvasResizeMode :: Lens' CairoRenderState CanvasResizeMode
canvasResizeMode = lens theCanvasResizeMode $ \ a b -> a{ theCanvasResizeMode = b }

cairoScreenPrinterState :: Lens' CairoRenderState ScreenPrinterState
cairoScreenPrinterState = lens theCairoScreenPrinterState $ \ a b ->
  a{ theCairoScreenPrinterState = b }

-- | Lift a @"Graphics.Rendering.Cairo".'Cairo.Render'@ function type into the 'CairoRender'
-- function type.
cairoRender :: Cairo.Render a -> CairoRender a
cairoRender = CairoRender . lift

-- | Extract a @"Graphics.Rendering.Cairo".'Cairo.Render'@ from a 'CairoRender' function type.
--
-- This function requries you to pass the desired pixel buffer size, which is usually stored as part
-- of the data structure that contains the canvas (or stored by the operating system window manager
-- as it keeps track of window sizes). Internally, the 'CairoRenderState' keeps track of it's own
-- windows size. When this function is evalauted, if the window size you pass here is different from
-- the window size tracked in the 'CairoRenderState', a decision is made based on the value of
-- 'canvasResizeMode' as to whether a new pixel buffer of the new size needs to be allocated and the
-- old pixel buffer needs to be blitted to the new pixel buffer before evaluating the given image
-- drawing function.
runCairoRender
  :: PixSize -> CairoRenderState -> CairoRender a -> Cairo.Render (a, CairoRenderState)
runCairoRender winsize rendst (CairoRender render) =
  flip runStateT (rendst & cairoKeepWinSize .~ winsize) $ do
    style <- use $ cairoScreenPrinterState . fontStyle
    lift $ cairoSetFontStyle True style style
    render

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

-- | Switch to raster mode (only if it isn't already) which flushes all pending vector events. This
-- function is called before every raster operation.
rasterMode :: Double -> Double -> CairoRender ()
rasterMode x y = CairoRender $ use cairoRenderMode >>= \ case
  RasterMode (V2 loX loY) (V2 hiX hiY) -> do
    cairoRenderMode .= RasterMode (V2 (min loX x) (min loY y)) (V2 (max hiX x) (max hiY y))
  VectorMode                           -> do
    lift cairoFlush
    cairoRenderMode .= RasterMode (V2 x y) (V2 x y)

-- | Set a 'Happlets.Draw.Text.FontStyle' in the current 'Cairo.Render' context.
cairoSetFontStyle :: Bool -> FontStyle -> FontStyle -> Cairo.Render FontStyle
cairoSetFontStyle force oldStyle newStyle0 = do
  logIO <- mkLogger "cairoSetFontStyle"
  let newStyle = newStyle0 & fontSize %~ max 6.0 . min 600.0
  let changed :: Eq a => (FontStyle -> a) -> Bool
      changed getter = force || getter oldStyle /= getter newStyle
  if changed theFontSize || changed theFontBold || changed theFontItalic
   then do
    when (changed theFontBold || changed theFontItalic) $ do
      liftIO $ logIO _textevt $ "Cairo.selectFontFace \"monospace\" " ++
        (if theFontItalic newStyle then "(italic)" else "(not italic)") ++
        (if theFontBold   newStyle then "(bold)"   else "(not bold)")
      Cairo.selectFontFace ("monospace" :: Strict.Text)
        (if theFontItalic newStyle then Cairo.FontSlantItalic else Cairo.FontSlantNormal )
        (if theFontBold   newStyle then Cairo.FontWeightBold  else Cairo.FontWeightNormal)
    when (changed theFontSize) $ do
      liftIO $ logIO _textevt $ "Cairo.setFontSize "++show (theFontSize newStyle)
      Cairo.setFontSize $ theFontSize newStyle
   else liftIO $ logIO _textevt
          "-- fontSize, fontItalic, fontBold, values are all the same as before"
  return newStyle

-- not for export
-- This does NOT evaluate 'spanPrintable' on the input 'String' so it must not be exported.
cairoPrintNoAdvance :: Bool -> PrintableString -> CairoRender (Maybe TextBoundingBox)
cairoPrintNoAdvance doDisplay = unwrapPrintable >>> \ str -> do
  logIO <- mkLogger "cairoPrintNoAdvance"
  st <- CairoRender $ use cairoScreenPrinterState
  let style = theFontStyle st
  let (TextGridLocation (TextGridRow row) (TextGridColumn col)) = theTextCursor st
  (V2 gridW gridH) <- getGridCellSizeDouble
  (V2 gridX gridY) <- gridLocationToPoint $ theTextCursor st
  liftIO $ logIO _textevt
    $ "cursor: row="++show row++", col="++show col
    ++", gridW="++show gridW++", gridH="++show gridH
    ++", gridX="++show gridX++", gridY="++show gridY
  cairoRender $ do
    ext <- Cairo.textExtents str
    liftIO $ logIO _textevt $ "Xadvance="++show (Cairo.textExtentsXadvance ext)++
      ", Yadvance="++show (Cairo.textExtentsYadvance ext)++
      ", Ybearing="++show (Cairo.textExtentsYbearing ext)++
      ", textWidth="++show (Cairo.textExtentsWidth ext)++
      ", textHeight="++show (Cairo.textExtentsHeight ext)
    let pt   = V2 (realToFrac col * gridW) (realToFrac row * gridH) + theRenderOffset st
    let size = V2 (Cairo.textExtentsXadvance ext + 2.0) gridH
    let rect = Rect2D pt (pt + size)
    liftIO $ logIO _textevt $ "Cairo.textExtents -> " ++ show size ++ ", cursor=" ++ show pt
    when doDisplay $ do
      liftIO $ logIO _textevt $ "cairoDrawRect "
        ++ show (theFontBackColor style)
        ++ ' ' : show (rect ^. rect2DPoints) ++ " -- background"
      cairoDrawRect (theFontBackColor style) (0::Double) (theFontBackColor style) rect
      liftIO $ logIO _textevt $ "moveTo "
        ++ show gridX       ++ ' ':show gridY
        ++ " setColor "    ++ show (theFontForeColor style)
        ++ " setFontSize " ++ show (theFontSize      style)
      Cairo.moveTo gridX gridY
      cairoSetColor $ theFontForeColor style
      liftIO $ logIO _textevt $ "Cairo.showText "++show str
      Cairo.showText str
      Cairo.fill
    return $ Just rect

withCairoFontExtents :: (Double -> Double -> Double -> Double -> a) -> CairoRender a
withCairoFontExtents f = do
  logIO <- mkLogger "withCarioFontExtents"
  ext <- CairoRender $ lift Cairo.fontExtents
  let ascent     = Cairo.fontExtentsAscent  ext
  let descent    = Cairo.fontExtentsDescent ext
  let fontHeight = ascent + descent
  let fontWidth  = Cairo.fontExtentsMaxXadvance ext
  liftIO $ logIO _textevt $
    "fontHeight="++show fontHeight++", fontWidth="++show fontWidth++
    ", ascent="++show ascent++", descent="++show descent
  return $ f fontWidth fontHeight ascent descent

getGridCellSizeDouble :: CairoRender (Size2D Double)
getGridCellSizeDouble = withCairoFontExtents $ \ fontWidth fontHeight _ascent _descent -> 
  V2 fontWidth fontHeight

cairoGridLocationOfPoint :: Point2D Double -> CairoRender TextGridLocation
cairoGridLocationOfPoint pt = do
  off <- CairoRender $ use $ cairoScreenPrinterState . renderOffset
  let (V2 x y) = pt - off
  withCairoFontExtents $ \ fontWidth fontHeight _ascent _descent -> TextGridLocation
    (TextGridRow    $ floor $ y / fontHeight)
    (TextGridColumn $ floor $ x / fontWidth)

cairoArray :: (Int -> Int -> Cairo.SurfaceData Int Word32 -> IO a) -> Cairo.Render a
cairoArray f = Cairo.withTargetSurface $ \ surface -> do
  w <- Cairo.imageSurfaceGetWidth surface
  h <- Cairo.imageSurfaceGetHeight surface
  liftIO $ Cairo.imageSurfaceGetPixels surface >>= f w h

pointToInt :: RealFrac n => Int -> Int -> Point2D n -> Int
pointToInt w _h pt = let (x, y) = (round <$> pt) ^. pointXY in y*w + x

-- | The implementation of 'Happlets.Draw.getPoint' for the 'Cairo.Render' function type.
cairoGetPoint :: RealFrac n => Point2D n -> Cairo.Render Color
cairoGetPoint pt = cairoArray $ \ w h surfaceData ->
  liftIO $ set32BitsARGB <$> readArray surfaceData (pointToInt w h pt)

-- | Call this function at least once before calling 'cairoSetPoint'. Note that if you use a
-- 'CairoRender' function type and the ordinary 'Happlets.Draw.setPoint' function in the
-- "Happlets.Draw" module, this function never needs to be called, the internal state of the
-- 'CairoRender' function tracks when to flush and when to invalidate.
cairoFlush :: Cairo.Render ()
cairoFlush = Cairo.withTargetSurface Cairo.surfaceFlush

cairoResetAntialiasing :: Cairo.Render ()
cairoResetAntialiasing = do
  Cairo.setAntialias Cairo.AntialiasNone
  Cairo.setTolerance 1.0

-- | Force a single pixel at a given location to change to the given color.
cairoSetPoint :: RealFrac n => Point2D n -> Color -> Cairo.Render ()
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

cairoGradStops :: Cairo.Pattern -> [GradientStop] -> Cairo.Render ()
cairoGradStops pat = mapM_ $ \ stop -> do
  let (r,g,b,a) = unpackRGBA32Color $ stop ^. gradStopColor
  Cairo.patternAddColorStopRGBA pat (stop ^. gradStopPoint) r g b a

m44toCairoMatrix :: RealFrac n => Transform2D n -> Cairo.Matrix
m44toCairoMatrix m0 = let (V2 (V2 xx yx) (V2 xy yy)) = fmap realToFrac <$> (m0 ^. _m22) in
  Cairo.Matrix xx yx xy yy 0 0

cairoDrawWithSource :: Lens' CairoRenderState PaintSource -> Cairo.Render () -> CairoRender ()
cairoDrawWithSource paint draw = let f = realToFrac in do
  use paint >>= cairoRender . \ case
    PaintSolidColor    color           -> cairoSetColor color
    PaintGradient gtyp start end stops -> do
      let gradStops pat = cairoGradStops pat $
            GradientStop 0.0 start : gradStopsToList stops ++ [GradientStop 1.0 end]
      case gtyp of
        GradRadial (V2 x1 y1) (Magnitude r1) (V2 x2 y2) (Magnitude r2) ->
          Cairo.withRadialPattern (f x1) (f y1) (f r1) (f x2) (f y2) (f r2) gradStops
        GradLinear (V2 x1 y1) (V2 x2 y2) ->
          Cairo.withLinearPattern (f x1) (f y1) (f x2) (f y2) gradStops
  geom <- use cairoGeometry
  vectorMode
  cairoRender $ do
    Cairo.setLineWidth $ geom ^. cairoLineWidth
    Cairo.setMatrix $ m44toCairoMatrix $ geom ^. cairoBlitTransform
    cairoDrawShape $ geom ^. cairoShape
    draw

-- | Move the position of the cairo graphics context "pen" object.
cairoMoveTo :: RealFrac n => Point2D n -> Cairo.Render ()
cairoMoveTo = uncurry Cairo.moveTo . view pointXY . fmap realToFrac

-- | Using the cairo graphics context current color, and the position of the "pen" object, draw a
-- line from the current pen position to the given point.
cairoLineTo :: RealFrac n => Point2D n -> Cairo.Render ()
cairoLineTo = uncurry Cairo.lineTo . view pointXY . fmap realToFrac

-- | Draw a single line of the given color. This will also draw the line caps at the start and end
-- points.
cairoDrawLine
  :: (RealFrac n, RealFrac lw)
  => LineColor -> LineWidth lw -> Line2D n -> Cairo.Render ()
cairoDrawLine color width line = do
  cairoSetColor color
  Cairo.setLineCap Cairo.LineCapRound
  Cairo.setLineWidth $ realToFrac width
  cairoMoveTo $ line ^. line2DHead
  cairoLineTo $ line ^. line2DTail
  Cairo.stroke

-- | Similar to 'cairoDrawLine' but draws multiple line segments, each next segment beginning where
-- the previous segment ended. The line is drawn with the given color. Only two line caps are drawn:
-- one at the first given point and one at the last given point in the list of points.
cairoDrawPath
  :: (RealFrac n, RealFrac lw)
  => LineColor -> LineWidth lw -> [Point2D n] -> Cairo.Render ()
cairoDrawPath color width =
  let run a ax = do
        cairoSetColor color
        Cairo.setLineCap Cairo.LineCapRound
        Cairo.setLineWidth $ realToFrac width
        Cairo.setLineJoin Cairo.LineJoinMiter
        cairoMoveTo a
        forM_ ax cairoLineTo
        Cairo.stroke
  in  \ case { [] -> return (); [a] -> run a [a]; a:ax -> run a ax; }

cairoRectangle :: RealFrac n => Rect2D n -> Cairo.Render ()
cairoRectangle rect = do
  cairoMoveTo $ rect ^. rect2DHead
  let (head, tail) = (realToFrac <$> canonicalRect2D rect) ^. rect2DPoints
  let (x, y) = head ^. pointXY
  let (w, h) = (tail - head) ^. pointXY
  Cairo.rectangle x y w h

cairoDrawRect
  :: (RealFrac n, RealFrac lw)
  => LineColor -> LineWidth lw -> FillColor -> Rect2D n -> Cairo.Render ()
cairoDrawRect lineColor width fillColor rect = do
  cairoSetColor fillColor
  cairoRectangle rect
  Cairo.fill
  when (width > 0.0) $ do
    cairoSetColor lineColor
    Cairo.setLineWidth $ realToFrac width
    Cairo.setLineJoin Cairo.LineJoinMiter
    cairoRectangle rect
    Cairo.stroke

cairoArc :: RealFrac n => Arc2D n -> Cairo.Render ()
cairoArc arc0 = do
  let arc = realToFrac <$> arc0
  let (V2 x y) = arc ^. arc2DOrigin
  let (Magnitude r) = arc ^. arc2DRadius
  let (Angle start) = arc ^. arc2DStart
  let (Angle   end) = arc ^. arc2DEnd
  Cairo.arc x y r start end

cairoPath :: (UVec.Unbox n, RealFrac n) => Path2D n -> Cairo.Render ()
cairoPath = path2DPoints >>> \ (init, points) -> do
  cairoMoveTo init
  forM_ points cairoLineTo

cairoCubic :: (UVec.Unbox n, RealFrac n) => Cubic2D n -> Cairo.Render ()
cairoCubic = cubic2DPoints >>> \ (init, segments) -> do
  cairoMoveTo init
  forM_ (fmap realToFrac <$> segments) $ \ seg -> do
    let (V2 ctrl1X ctrl1Y) = seg ^. cubic2DCtrlPt1
    let (V2 ctrl2X ctrl2Y) = seg ^. cubic2DCtrlPt2
    let (V2   endX   endY) = seg ^. cubic2DEndPoint
    Cairo.curveTo ctrl1X ctrl1Y ctrl2X ctrl2Y endX endY

cairoDrawShape :: (UVec.Unbox n, RealFrac n) => Draw2DShape n -> Cairo.Render ()
cairoDrawShape = \ case
  Draw2DReset -> return ()
  Draw2DLine (Line2D from to) -> cairoMoveTo from >> cairoLineTo to
  Draw2DRect  rect -> cairoRectangle rect
  Draw2DArc   arc  -> cairoArc arc
  Draw2DPath  path -> cairoPath path
  Draw2DCubic path -> cairoCubic path

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

----------------------------------------------------------------------------------------------------

instance RenderText CairoRender where
  getRendererFontStyle = CairoRender $ use $ cairoScreenPrinterState . fontStyle
  setRendererFontStyle newStyle = CairoRender $ do
    oldStyle <- use $ cairoScreenPrinterState . fontStyle
    newStyle <- lift $ cairoSetFontStyle False oldStyle newStyle
    cairoScreenPrinterState . fontStyle .= newStyle
    return newStyle

  getGridCellSize = fmap realToFrac <$> getGridCellSizeDouble
  getWindowTextGridSize = do
    (V2 txtW txtH) <- getGridCellSizeDouble
    (V2 winW winH) <- CairoRender $ use cairoKeepWinSize
    return $ TextGridLocation
      (TextGridRow    $ ceiling $ realToFrac winH / txtH)
      (TextGridColumn $ ceiling $ realToFrac winW / txtW)
    
  gridLocationOfPoint = cairoGridLocationOfPoint . fmap realToFrac
  gridLocationToPoint (TextGridLocation (TextGridRow row) (TextGridColumn col)) = do
    off <- CairoRender $ use $ cairoScreenPrinterState . renderOffset
    withCairoFontExtents $ \ fontWidth fontHeight ascent _descent -> off + V2
      (realToFrac col * fontWidth + 1.5)
      (fontHeight * realToFrac row + ascent + 0.5)

  screenPrintNoAdvance = cairoPrintNoAdvance True
  screenPrintCharNoAdvance = maybe (pure Nothing) (cairoPrintNoAdvance True) . printableChar

  getStringBoundingBox = cairoPrintNoAdvance False
  getCharBoundingBox = maybe (pure Nothing) (cairoPrintNoAdvance False) . printableChar

  getScreenPrinterState = CairoRender $ use cairoScreenPrinterState
  setScreenPrinterState st = CairoRender $ do
    cairoScreenPrinterState .= st
    logIO <- mkLogger "saveScreenPrinterState"
    let (TextGridLocation (TextGridRow row) (TextGridColumn col)) = st ^. textCursor
    liftIO $ logIO _textevt $ "row="++show row++", col="++show col

--cairoSelectFont :: SelectFont -> GtkGUI model (Maybe FontExtents)
--cairoSelectFont font0 = liftGtkStateIntoGUI $ evalCairo $ do
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


--  -- | A 'GtkDrawing' is a function that produces a 'Diagrams.Core.Types.Diagram' from a
--  -- 'Diagrams.BoundingBox.BoundingBox'. Use the 'Diagrams.BoundingBox.BoundingBox' information to
--  -- inform the placement and scale of your diagram.
--  type GtkCairoDiagram = BoundingBox V2 Double -> Diagram Cairo
