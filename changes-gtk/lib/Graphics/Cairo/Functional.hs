module Graphics.Cairo.Functional
    ( CairoError
    , Drawing
    , drawingRender
    , drawingPoint
    , PixelPoint
    , pixelToAbstract
    , abstractToPixel
    , pointDrawing
    , ifPoint
    -- * Drawing
    , stroke
    , lineWidth
    , sourceRGB
    , lineCapSquare
    , operatorOver
        -- * Paths
    , Path
    , closePath
    , moveTo
    , lineTo
    , arc
    , arcNegative
        -- * Transformations
    , translate
    , rotate
    , scale
    ) where

import qualified GI.Cairo.Render as R
import qualified GI.Cairo.Render.Matrix as RM
import Shapes hiding (rotate)
import Shapes.Numeric

newtype CairoError =
    MkCairoError R.Status

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

drawingPoint :: Drawing a -> a
drawingPoint drawing = drawingItem drawing RM.identity

type PixelPoint = (Double, Double)

pixelToAbstract :: Drawing (PixelPoint -> (Double, Double))
pixelToAbstract = MkDrawing (return ()) $ \m -> RM.transformPoint (RM.invert m)

abstractToPixel :: Drawing ((Double, Double) -> PixelPoint)
abstractToPixel = MkDrawing (return ()) RM.transformPoint

pointDrawing :: ((Double, Double) -> a) -> Drawing (PixelPoint -> a)
pointDrawing f = fmap (\t -> f . t) pixelToAbstract

renderDrawing :: Monoid a => R.Render () -> Drawing a
renderDrawing r = mempty {drawingRender = checkStatus r}

dcontext :: R.Render () -> Drawing a -> Drawing a
dcontext change (MkDrawing render mp) = MkDrawing (rcontext $ checkStatus change >> render) mp

ifPoint ::
       forall a. Monoid a
    => ((Double, Double) -> Bool)
    -> Drawing (PixelPoint -> a)
    -> Drawing (PixelPoint -> a)
ifPoint test drawing =
    liftA2
        (\t a p ->
             if t p
                 then a p
                 else mempty)
        (pointDrawing test)
        drawing

lineWidth :: forall a. Double -> Drawing a -> Drawing a
lineWidth w = dcontext $ R.setLineWidth w

sourceRGB :: forall a. (Double, Double, Double) -> Drawing a -> Drawing a
sourceRGB (r, g, b) = dcontext $ R.setSourceRGB r g b

lineCapSquare :: forall a. Drawing a -> Drawing a
lineCapSquare = dcontext $ R.setLineCap R.LineCapSquare

operatorOver :: forall a. Drawing a -> Drawing a
operatorOver = dcontext $ R.setOperator $ R.OperatorOver

-- Paths
newtype Path = MkPath
    { unPath :: R.Render ()
    }

instance Semigroup Path where
    MkPath ra <> MkPath rb = MkPath $ ra >> rb

instance Monoid Path where
    mempty = MkPath $ return ()

closePath :: Path
closePath = MkPath $ R.closePath

arc :: Double -> Double -> Double -> Double -> Double -> Path
arc xc yc r a1 a2 = MkPath $ R.arc xc yc r a1 a2

arcNegative :: Double -> Double -> Double -> Double -> Double -> Path
arcNegative xc yc r a1 a2 = MkPath $ R.arcNegative xc yc r a1 a2

lineTo :: Double -> Double -> Path
lineTo x y = MkPath $ R.lineTo x y

moveTo :: Double -> Double -> Path
moveTo x y = MkPath $ R.moveTo x y

stroke ::
       forall a. Monoid a
    => [Path]
    -> Drawing a
stroke pp =
    renderDrawing $ do
        R.newPath
        unPath $ mconcat pp
        R.stroke

-- Transformations
saveMatrix :: forall a. R.Render a -> R.Render a
saveMatrix render = do
    m <- R.getMatrix
    a <- render
    R.setMatrix m
    return a

translate :: forall a. (Double, Double) -> Drawing a -> Drawing a
translate (x, y) (MkDrawing render mp) = let
    render' = saveMatrix $ checkStatus (R.translate x y) >> render
    mp' m = mp $ (RM.translate x y RM.identity) * m
    in MkDrawing render' mp'

rotate :: forall a. Double -> Drawing a -> Drawing a
rotate angle (MkDrawing render mp) = let
    render' = saveMatrix $ R.rotate angle >> render
    mp' m = mp $ (RM.rotate angle RM.identity) * m
    in MkDrawing render' mp'

scale :: forall a. (Double, Double) -> Drawing a -> Drawing a
scale (sx, sy) (MkDrawing render mp) = let
    render' = saveMatrix $ R.scale sx sy >> render
    mp' m = mp $ (RM.scale sx sy RM.identity) * m
    in MkDrawing render' mp'
