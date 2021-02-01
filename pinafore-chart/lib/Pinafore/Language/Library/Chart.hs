{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Chart
    ( chartLibrary
    ) where

import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Data.Shim
import Diagrams.Backend.Cairo as D
import Diagrams.Backend.Cairo.Internal as D
import Diagrams.Core as D
import Diagrams.Prelude as D
import Graphics.Rendering.Chart as C
import Graphics.Rendering.Chart.Backend.Diagrams as C
import Language.Expression.Dolan
import Pinafore.Language.API
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

-- from https://github.com/timbod7/haskell-chart/wiki/example-1
testChart :: C.Renderable ()
testChart = toRenderable layout
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

data ChartContext = MkChartContext
    { ccFontSelector :: FontSelector Double
    }

mkChartContext :: IO ChartContext
mkChartContext = do
    ccFontSelector <- loadCommonFonts
    return MkChartContext {..}

ccDEnv :: ChartContext -> (Double, Double) -> DEnv Double
ccDEnv MkChartContext {..} (w, h) = createEnv bitmapAlignmentFns w h ccFontSelector

diagramToDrawing :: Monoid m => (Double, Double) -> QDiagram Cairo V2 Double m -> LangDrawing
diagramToDrawing (w, h) diagram = let
    _cairoFileName = ""
    _cairoSizeSpec = mkSizeSpec2D (Just w) (Just h)
    _cairoOutputType = RenderOnly
    _cairoBypassAdjust = False
    options :: Options Cairo V2 Double
    options = CairoOptions {..}
    in MkLangDrawing $ snd $ renderDia Cairo options diagram

chartToDiagram :: ChartContext -> (Double, Double) -> LangChart -> QDiagram Cairo V2 Double Any
chartToDiagram cc sz chart = fst $ runBackendR (ccDEnv cc sz) chart

chartToDrawing :: ChartContext -> LangChart -> (Double, Double) -> LangDrawing
chartToDrawing cc chart sz = diagramToDrawing sz $ chartToDiagram cc sz chart

drawChart :: IO (LangChart -> (Double, Double) -> LangDrawing)
drawChart = do
    cc <- mkChartContext
    return $ chartToDrawing cc

chartLibraryModule :: LibraryModule
chartLibraryModule =
    MkDocTree
        "Chart"
        "Drawing charts."
        [ mkTypeEntry "Chart" "A chart." $ MkBoundType chartGroundType
        , mkValEntry "testChart" "A test chart." testChart
        , mkValEntry "drawChart" "Draw a chart." drawChart
        ]

chartLibrary :: [LibraryModule]
chartLibrary = [chartLibraryModule]
