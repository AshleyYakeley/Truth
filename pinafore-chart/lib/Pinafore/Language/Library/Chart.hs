{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Chart
    ( chartLibrary
    ) where

import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Data.Shim
import Graphics.Rendering.Chart as C
import Graphics.Rendering.Chart.Backend.Diagrams as C
import Language.Expression.Dolan
import Pinafore.Language.API
import Pinafore.Language.Library.Diagram
import Pinafore.Language.Library.GTK
import Shapes
import Shapes.Numeric

type LangChart = C.Renderable ()

chartGroundType :: PinaforeGroundType '[] LangChart
chartGroundType =
    SimpleGroundType NilListType NilDolanVarianceMap ("Chart", 0) $
    MkProvidedType $(iowitness [t|'MkWitKind (HetEqual LangChart)|]) HetRefl

-- LangChart
instance ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) LangChart where
    toShimWit = mkShimWit $ GroundDolanSingularType chartGroundType NilDolanArguments

instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) LangChart where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) LangChart where
    fromShimWit = mkShimWit $ GroundDolanSingularType chartGroundType NilDolanArguments

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) LangChart where
    fromShimWit = singleDolanShimWit fromJMShimWit

data ChartContext = MkChartContext
    { ccFontSelector :: FontSelector Double
    }

mkChartContext :: IO ChartContext
mkChartContext = do
    ccFontSelector <- loadCommonFonts
    return MkChartContext {..}

chartToDiagram :: ChartContext -> (Double, Double) -> LangChart -> LangDiagram
chartToDiagram MkChartContext {..} (w, h) chart = let
    env :: DEnv Double
    env = createEnv bitmapAlignmentFns w h ccFontSelector
    in fst $ runBackendR env chart

chartToDrawing :: ChartContext -> LangChart -> (Double, Double) -> LangDrawing
chartToDrawing cc chart sz = diagramToDrawing sz $ chartToDiagram cc sz chart

getDraw :: IO (LangChart -> (Double, Double) -> LangDrawing)
getDraw = do
    cc <- mkChartContext
    return $ chartToDrawing cc

-- from https://github.com/timbod7/haskell-chart/wiki/example-1
test :: LangChart
test = toRenderable layout
  where
    am :: Double -> Double
    am x = (sin (x * 3.14159 / 45) + 1) / 2 * (sin (x * 3.14159 / 5))
    sinusoid1 =
        plot_lines_values .~ [[(x, (am x)) | x <- [0,(0.5) .. 400]]] $ plot_lines_style . line_color .~ opaque blue $
        plot_lines_title .~
        "am" $
        def
    sinusoid2 =
        plot_points_style .~ filledCircles 2 (opaque red) $ plot_points_values .~ [(x, (am x)) | x <- [0,7 .. 400]] $
        plot_points_title .~
        "am points" $
        def
    layout = layout_title .~ "Amplitude Modulation" $ layout_plots .~ [toPlot sinusoid1, toPlot sinusoid2] $ def

chartLibraryModule :: LibraryModule
chartLibraryModule =
    MkDocTree
        "Chart"
        "Drawing charts."
        [ mkTypeEntry "Chart" "A chart." $ MkBoundType chartGroundType
        , mkValEntry
              "getDraw"
              "Initialise the Chart module to get a chart-drawing function. May take several seconds."
              getDraw
        , mkValEntry "test" "A test chart." test
        ]

chartLibrary :: [LibraryModule]
chartLibrary = [chartLibraryModule]
