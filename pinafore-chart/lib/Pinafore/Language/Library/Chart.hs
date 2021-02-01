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
import Pinafore.Language.Library.Chart.Plot
import Pinafore.Language.Library.Diagram
import Pinafore.Language.Library.GTK
import Shapes
import Shapes.Numeric

type LangRenderable = C.Renderable ()

renderableGroundType :: PinaforeGroundType '[] LangRenderable
renderableGroundType =
    SimpleGroundType NilListType NilDolanVarianceMap ("Renderable", 0) $
    MkProvidedType $(iowitness [t|'MkWitKind (HetEqual LangRenderable)|]) HetRefl

-- LangRenderable
instance ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) LangRenderable where
    toShimWit = mkShimWit $ GroundDolanSingularType renderableGroundType NilDolanArguments

instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) LangRenderable where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) LangRenderable where
    fromShimWit = mkShimWit $ GroundDolanSingularType renderableGroundType NilDolanArguments

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) LangRenderable where
    fromShimWit = singleDolanShimWit fromJMShimWit

data ChartContext = MkChartContext
    { ccFontSelector :: FontSelector Double
    }

mkChartContext :: IO ChartContext
mkChartContext = do
    ccFontSelector <- loadCommonFonts
    return MkChartContext {..}

chartToDiagram :: ChartContext -> (Double, Double) -> LangRenderable -> LangDiagram
chartToDiagram MkChartContext {..} (w, h) chart = let
    env :: DEnv Double
    env = createEnv bitmapAlignmentFns w h ccFontSelector
    in fst $ runBackendR env chart

chartToDrawing :: ChartContext -> LangRenderable -> (Double, Double) -> LangDrawing
chartToDrawing cc chart sz = diagramToDrawing sz $ chartToDiagram cc sz chart

getDraw :: IO (LangRenderable -> (Double, Double) -> LangDrawing)
getDraw = do
    cc <- mkChartContext
    return $ chartToDrawing cc

-- from https://github.com/timbod7/haskell-chart/wiki/example-1
test :: LangRenderable
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
        [ mkValEntry
              "getDraw"
              "Initialise the Chart module to get a rendering function. May take several seconds."
              getDraw
        , mkValEntry "test" "A test chart." test
        , docTreeEntry
              "Renderable"
              ""
              [mkTypeEntry "Renderable" "Something that can be rendered." $ MkBoundType renderableGroundType]
        , plotStuff
        ]

chartLibrary :: [LibraryModule]
chartLibrary = [chartLibraryModule]
