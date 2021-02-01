module Pinafore.Language.Library.Diagram where

import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Diagrams.Core
import Diagrams.Prelude
import Pinafore.Language.Library.GTK
import Shapes
import Shapes.Numeric

type LangDiagram = QDiagram Cairo V2 Double Any

diagramToDrawing :: (Double, Double) -> LangDiagram -> LangDrawing
diagramToDrawing (w, h) diagram = let
    _cairoFileName = ""
    _cairoSizeSpec = mkSizeSpec2D (Just w) (Just h)
    _cairoOutputType = RenderOnly
    _cairoBypassAdjust = False
    options :: Options Cairo V2 Double
    options = CairoOptions {..}
    in MkLangDrawing $ snd $ renderDia Cairo options diagram
