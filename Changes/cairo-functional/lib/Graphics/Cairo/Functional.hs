module Graphics.Cairo.Functional
    ( -- * Drawing
      CairoError
    , Drawing
    , drawingRender

      -- * Clicks
    , PixelPoint
    , pixelToAbstract
    , abstractToPixel
    , pointDrawing
    , drawingPoint
    , ifPoint
    , ifInRect

      -- * Co-ordinate Transformation
    , translate
    , rotate
    , scale

      -- * Painting
    , paint
    , paintWithAlpha
    , sourceRGB
    , sourceRGBA
    , operatorClear
    , operatorSource
    , operatorOver
    , operatorIn
    , operatorOut
    , operatorAtop
    , operatorDest
    , operatorDestOver
    , operatorDestIn
    , operatorDestOut
    , operatorDestAtop
    , operatorXor
    , operatorAdd
    , operatorSaturate
    , operatorMultiply
    , operatorScreen
    , operatorOverlay
    , operatorDarken
    , operatorLighten
    , operatorColorDodge
    , operatorColorBurn
    , operatorHardLight
    , operatorSoftLight
    , operatorDifference
    , operatorExclusion
    , operatorHslHue
    , operatorHslSaturation
    , operatorHslColor
    , operatorHslLuminosity

      -- * Paths
    , Path
    , stroke
    , fill
    , clip

      -- ** Construction
    , closePath
    , moveTo
    , lineTo
    , curveTo
    , relMoveTo
    , relLineTo
    , relCurveTo
    , rectangle
    , arc
    , arcNegative

      -- ** Properties
    , lineWidth
    , lineJoinMitre
    , lineJoinRound
    , lineJoinBevel
    , lineCapButt
    , lineCapRound
    , lineCapSquare
    , dash
    , fillRuleWinding
    , fillRuleEvenOdd

      -- * Text
    , textPath
    , R.FontSlant (..)
    , R.FontWeight (..)
    , fontFace
    , fontSize

      -- * Patterns
    , Pattern
    , patternSource
    , patternMask
    , rgbPattern
    , rgbaPattern
    , PatternColorStop (..)
    , linearPattern
    , radialPattern

      -- ** From Drawings
    , colorDrawingPattern
    , alphaDrawingPattern
    , colorAlphaDrawingPattern
    )
where

import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Matrix qualified as RM
import Shapes hiding (rotate)
import Shapes.Numeric

--
-- Drawing
--
newtype CairoError
    = MkCairoError R.Status

instance Show CairoError where
    show (MkCairoError s) = "Cairo: " <> show s

instance Exception CairoError

checkStatus :: R.Render a -> R.Render a
checkStatus r = do
    a <- r
    s <- R.status
    case s of
        R.StatusSuccess -> return a
        _ -> liftIO $ throw $ MkCairoError s

rcontext :: R.Render a -> R.Render a
rcontext r = do
    R.save
    a <- r
    R.restore
    return a

data Drawing a = MkDrawing
    { drawingRender :: R.Render ()
    , drawingItem :: R.Matrix -> a
    }

instance Functor Drawing where
    fmap ab (MkDrawing render mp) = MkDrawing render $ fmap ab mp

instance Applicative Drawing where
    pure a = MkDrawing (return ()) $ \_ -> a
    MkDrawing ra ia <*> MkDrawing rb ib = MkDrawing (ra >> rb) $ ia <*> ib

instance Semigroup a => Semigroup (Drawing a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Drawing a) where
    mempty = pure mempty

--
-- Clicks
--
type PixelPoint = (Double, Double)

pixelToAbstract :: Drawing (PixelPoint -> (Double, Double))
pixelToAbstract = MkDrawing (return ()) $ \m -> RM.transformPoint (RM.invert m)

abstractToPixel :: Drawing ((Double, Double) -> PixelPoint)
abstractToPixel = MkDrawing (return ()) RM.transformPoint

pointDrawing :: ((Double, Double) -> a) -> Drawing (PixelPoint -> a)
pointDrawing f = fmap (\t -> f . t) pixelToAbstract

drawingPoint :: Drawing a -> a
drawingPoint drawing = drawingItem drawing RM.identity

renderDrawing :: Monoid a => R.Render () -> Drawing a
renderDrawing r = mempty{drawingRender = checkStatus r}

dcontext :: R.Render () -> Drawing a -> Drawing a
dcontext change (MkDrawing render mp) = MkDrawing (rcontext $ checkStatus change >> render) mp

ifPoint ::
    forall a.
    Monoid a =>
    ((Double, Double) -> Bool) ->
    Drawing (PixelPoint -> a) ->
    Drawing (PixelPoint -> a)
ifPoint test drawing =
    liftA2
        ( \t a p ->
            if t p
                then a p
                else mempty
        )
        (pointDrawing test)
        drawing

ifInRect ::
    forall a.
    Monoid a =>
    ((Double, Double), (Double, Double)) ->
    Drawing (PixelPoint -> a) ->
    Drawing (PixelPoint -> a)
ifInRect ((x0, y0), (w, h)) = ifPoint $ \(x, y) -> (x >= x0) && (x < x0 + w) && (y >= y0) && (y < y0 + h)

--
-- Co-ordinate Transformation
--
saveMatrix :: R.Render --> R.Render
saveMatrix render = do
    m <- R.getMatrix
    a <- render
    R.setMatrix m
    return a

translate :: (Double, Double) -> Drawing --> Drawing
translate (x, y) (MkDrawing render mp) = let
    render' = saveMatrix $ checkStatus (R.translate x y) >> render
    mp' m = mp $ (RM.translate x y RM.identity) * m
    in MkDrawing render' mp'

rotate :: Double -> Drawing --> Drawing
rotate angle (MkDrawing render mp) = let
    render' = saveMatrix $ R.rotate angle >> render
    mp' m = mp $ (RM.rotate angle RM.identity) * m
    in MkDrawing render' mp'

scale :: (Double, Double) -> Drawing --> Drawing
scale (sx, sy) (MkDrawing render mp) = let
    render' = saveMatrix $ R.scale sx sy >> render
    mp' m = mp $ (RM.scale sx sy RM.identity) * m
    in MkDrawing render' mp'

--
-- Painting
--
paint ::
    forall a.
    Monoid a =>
    Drawing a
paint = renderDrawing $ R.paint

paintWithAlpha ::
    forall a.
    Monoid a =>
    Double ->
    Drawing a
paintWithAlpha a = renderDrawing $ R.paintWithAlpha a

sourceRGB :: (Double, Double, Double) -> Drawing --> Drawing
sourceRGB (r, g, b) = dcontext $ R.setSourceRGB r g b

sourceRGBA :: ((Double, Double, Double), Double) -> Drawing --> Drawing
sourceRGBA ((r, g, b), a) = dcontext $ R.setSourceRGBA r g b a

operatorClear :: Drawing --> Drawing
operatorClear = dcontext $ R.setOperator $ R.OperatorClear

operatorSource :: Drawing --> Drawing
operatorSource = dcontext $ R.setOperator $ R.OperatorSource

operatorOver :: Drawing --> Drawing
operatorOver = dcontext $ R.setOperator $ R.OperatorOver

operatorIn :: Drawing --> Drawing
operatorIn = dcontext $ R.setOperator $ R.OperatorIn

operatorOut :: Drawing --> Drawing
operatorOut = dcontext $ R.setOperator $ R.OperatorOut

operatorAtop :: Drawing --> Drawing
operatorAtop = dcontext $ R.setOperator $ R.OperatorAtop

operatorDest :: Drawing --> Drawing
operatorDest = dcontext $ R.setOperator $ R.OperatorDest

operatorDestOver :: Drawing --> Drawing
operatorDestOver = dcontext $ R.setOperator $ R.OperatorDestOver

operatorDestIn :: Drawing --> Drawing
operatorDestIn = dcontext $ R.setOperator $ R.OperatorDestIn

operatorDestOut :: Drawing --> Drawing
operatorDestOut = dcontext $ R.setOperator $ R.OperatorDestOut

operatorDestAtop :: Drawing --> Drawing
operatorDestAtop = dcontext $ R.setOperator $ R.OperatorDestAtop

operatorXor :: Drawing --> Drawing
operatorXor = dcontext $ R.setOperator $ R.OperatorXor

operatorAdd :: Drawing --> Drawing
operatorAdd = dcontext $ R.setOperator $ R.OperatorAdd

operatorSaturate :: Drawing --> Drawing
operatorSaturate = dcontext $ R.setOperator $ R.OperatorSaturate

operatorMultiply :: Drawing --> Drawing
operatorMultiply = dcontext $ R.setOperator $ R.OperatorMultiply

operatorScreen :: Drawing --> Drawing
operatorScreen = dcontext $ R.setOperator $ R.OperatorScreen

operatorOverlay :: Drawing --> Drawing
operatorOverlay = dcontext $ R.setOperator $ R.OperatorOverlay

operatorDarken :: Drawing --> Drawing
operatorDarken = dcontext $ R.setOperator $ R.OperatorDarken

operatorLighten :: Drawing --> Drawing
operatorLighten = dcontext $ R.setOperator $ R.OperatorLighten

operatorColorDodge :: Drawing --> Drawing
operatorColorDodge = dcontext $ R.setOperator $ R.OperatorColorDodge

operatorColorBurn :: Drawing --> Drawing
operatorColorBurn = dcontext $ R.setOperator $ R.OperatorColorBurn

operatorHardLight :: Drawing --> Drawing
operatorHardLight = dcontext $ R.setOperator $ R.OperatorHardLight

operatorSoftLight :: Drawing --> Drawing
operatorSoftLight = dcontext $ R.setOperator $ R.OperatorSoftLight

operatorDifference :: Drawing --> Drawing
operatorDifference = dcontext $ R.setOperator $ R.OperatorDifference

operatorExclusion :: Drawing --> Drawing
operatorExclusion = dcontext $ R.setOperator $ R.OperatorExclusion

operatorHslHue :: Drawing --> Drawing
operatorHslHue = dcontext $ R.setOperator $ R.OperatorHslHue

operatorHslSaturation :: Drawing --> Drawing
operatorHslSaturation = dcontext $ R.setOperator $ R.OperatorHslSaturation

operatorHslColor :: Drawing --> Drawing
operatorHslColor = dcontext $ R.setOperator $ R.OperatorHslColor

operatorHslLuminosity :: Drawing --> Drawing
operatorHslLuminosity = dcontext $ R.setOperator $ R.OperatorHslLuminosity

--
-- Paths
--
newtype Path = MkPath
    { unPath :: R.Render ()
    }

instance Semigroup Path where
    MkPath ra <> MkPath rb = MkPath $ ra >> rb

instance Monoid Path where
    mempty = MkPath $ return ()

pathRender :: Path -> R.Render ()
pathRender pp = do
    R.newPath
    unPath pp

stroke ::
    forall a.
    Monoid a =>
    Path ->
    Drawing a
stroke pp =
    renderDrawing $ do
        pathRender pp
        R.stroke

fill ::
    forall a.
    Monoid a =>
    Path ->
    Drawing a
fill pp =
    renderDrawing $ do
        pathRender pp
        R.fill

clip :: Path -> Drawing --> Drawing
clip pp =
    dcontext $ do
        pathRender pp
        R.clip

-- Construction
closePath :: Path
closePath = MkPath $ R.closePath

moveTo :: (Double, Double) -> Path
moveTo (x, y) = MkPath $ R.moveTo x y

lineTo :: (Double, Double) -> Path
lineTo (x, y) = MkPath $ R.lineTo x y

curveTo :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Path
curveTo (x1, y1) (x2, y2) (x3, y3) = MkPath $ R.curveTo x1 y1 x2 y2 x3 y3

relMoveTo :: (Double, Double) -> Path
relMoveTo (x, y) = MkPath $ R.relMoveTo x y

relLineTo :: (Double, Double) -> Path
relLineTo (x, y) = MkPath $ R.relLineTo x y

relCurveTo :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Path
relCurveTo (x1, y1) (x2, y2) (x3, y3) = MkPath $ R.relCurveTo x1 y1 x2 y2 x3 y3

rectangle :: (Double, Double) -> (Double, Double) -> Path
rectangle (x, y) (w, h) = MkPath $ R.rectangle x y w h

arc :: (Double, Double) -> Double -> Double -> Double -> Path
arc (xc, yc) r a1 a2 = MkPath $ R.arc xc yc r a1 a2

arcNegative :: (Double, Double) -> Double -> Double -> Double -> Path
arcNegative (xc, yc) r a1 a2 = MkPath $ R.arcNegative xc yc r a1 a2

-- Properties
lineWidth :: Double -> Drawing --> Drawing
lineWidth w = dcontext $ R.setLineWidth w

lineJoinMitre :: Double -> Drawing --> Drawing
lineJoinMitre limit =
    dcontext $ do
        R.setLineJoin R.LineJoinMiter
        R.setMiterLimit limit

lineJoinRound :: Drawing --> Drawing
lineJoinRound = dcontext $ R.setLineJoin R.LineJoinRound

lineJoinBevel :: Drawing --> Drawing
lineJoinBevel = dcontext $ R.setLineJoin R.LineJoinBevel

lineCapButt :: Drawing --> Drawing
lineCapButt = dcontext $ R.setLineCap R.LineCapButt

lineCapRound :: Drawing --> Drawing
lineCapRound = dcontext $ R.setLineCap R.LineCapRound

lineCapSquare :: Drawing --> Drawing
lineCapSquare = dcontext $ R.setLineCap R.LineCapSquare

dash :: [Double] -> Double -> Drawing --> Drawing
dash dd offset = dcontext $ R.setDash dd offset

fillRuleWinding :: Drawing --> Drawing
fillRuleWinding = dcontext $ R.setFillRule R.FillRuleWinding

fillRuleEvenOdd :: Drawing --> Drawing
fillRuleEvenOdd = dcontext $ R.setFillRule R.FillRuleEvenOdd

--
-- Text
--
textPath :: Text -> Path
textPath t = MkPath $ R.textPath t

-- Text Attributes
fontFace :: Text -> R.FontSlant -> R.FontWeight -> Drawing --> Drawing
fontFace fname s w = dcontext $ R.selectFontFace fname s w

fontSize :: Double -> Drawing --> Drawing
fontSize s = dcontext $ R.setFontSize s

--
-- Patterns
--
newtype Pattern
    = MkPattern (forall a. (R.Pattern -> R.Render a) -> R.Render a)

patternSource :: Pattern -> Drawing a -> Drawing a
patternSource (MkPattern wpat) (MkDrawing render mp) =
    MkDrawing (wpat $ \pat -> rcontext $ checkStatus (R.setSource pat) >> render) mp

patternMask ::
    forall a.
    Monoid a =>
    Pattern ->
    Drawing a
patternMask (MkPattern wpat) = renderDrawing $ wpat $ \pat -> checkStatus $ R.mask pat

rgbPattern :: (Double, Double, Double) -> Pattern
rgbPattern (r, g, b) = MkPattern $ R.withRGBPattern r g b

rgbaPattern :: ((Double, Double, Double), Double) -> Pattern
rgbaPattern ((r, g, b), a) = MkPattern $ R.withRGBAPattern r g b a

data PatternColorStop
    = MkPatternColorStop
        Double
        ((Double, Double, Double), Maybe Double)

linearPattern :: [PatternColorStop] -> (Double, Double) -> (Double, Double) -> Pattern
linearPattern cstops (x0, y0) (x1, y1) =
    MkPattern $ \call ->
        R.withLinearPattern x0 y0 x1 y1 $ \pat -> do
            for_ cstops $ \(MkPatternColorStop offset ((r, g, b), ma)) ->
                case ma of
                    Nothing -> R.patternAddColorStopRGB pat offset r g b
                    Just a -> R.patternAddColorStopRGBA pat offset r g b a
            call pat

radialPattern :: [PatternColorStop] -> (Double, Double) -> Double -> (Double, Double) -> Double -> Pattern
radialPattern cstops (x0, y0) r0 (x1, y1) r1 =
    MkPattern $ \call ->
        R.withRadialPattern x0 y0 r0 x1 y1 r1 $ \pat -> do
            for_ cstops $ \(MkPatternColorStop offset ((r, g, b), ma)) ->
                case ma of
                    Nothing -> R.patternAddColorStopRGB pat offset r g b
                    Just a -> R.patternAddColorStopRGBA pat offset r g b a
            call pat

-- from Drawings
drawingPattern :: R.Content -> Drawing a -> Pattern
drawingPattern content drawing =
    MkPattern $ \call -> do
        R.pushGroupWithContent content
        drawingRender drawing
        R.withGroupPattern $ \pat -> call pat

colorDrawingPattern :: Drawing a -> Pattern
colorDrawingPattern = drawingPattern R.ContentColor

alphaDrawingPattern :: Drawing a -> Pattern
alphaDrawingPattern = drawingPattern R.ContentAlpha

colorAlphaDrawingPattern :: Drawing a -> Pattern
colorAlphaDrawingPattern = drawingPattern R.ContentColorAlpha
