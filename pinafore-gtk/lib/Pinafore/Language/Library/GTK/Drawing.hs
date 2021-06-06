{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.Drawing
    ( drawingLibraryModule
    , LangDrawing
    ) where

import Changes.UI.GTK
import Data.Shim
import Graphics.Cairo.Functional
import Language.Expression.Dolan
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Colour
import Shapes hiding (rotate)

-- LangDrawing
type LangDrawing = UIDrawing

drawingGroundType :: PinaforeGroundType '[] LangDrawing
drawingGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (HetEqual LangDrawing)|]) "Drawing"

instance ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) LangDrawing where
    toShimWit = mkShimWit $ GroundDolanSingularType drawingGroundType NilDolanArguments

instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) LangDrawing where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) LangDrawing where
    fromShimWit = mkShimWit $ GroundDolanSingularType drawingGroundType NilDolanArguments

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) LangDrawing where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- LangPath
type LangPath = Path

pathGroundType :: PinaforeGroundType '[] LangPath
pathGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (HetEqual LangPath)|]) "Path"

instance ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) LangPath where
    toShimWit = mkShimWit $ GroundDolanSingularType pathGroundType NilDolanArguments

instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) LangPath where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) LangPath where
    fromShimWit = mkShimWit $ GroundDolanSingularType pathGroundType NilDolanArguments

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) LangPath where
    fromShimWit = singleDolanShimWit fromJMShimWit

type UIP = PixelPoint -> UIEvents

setSourceColour :: LangAlphaColour -> LangDrawing -> LangDrawing
setSourceColour (MkLangAlphaColour op (MkSRGBColour r g b)) = sourceRGBA ((r, g, b), op)

drawingLibraryModule :: LibraryModule
drawingLibraryModule =
    MkDocTree
        "Drawing"
        ""
        [ mkTypeEntry "Drawing" "Something that can be drawn." $ MkBoundType drawingGroundType
        , mkSubtypeRelationEntry "[Drawing]" "Drawing" "Monoidal relationship" $
          pure $ monoidSubypeConversionEntry drawingGroundType
        , mkValEntry "stroke" "Draw a path" $ stroke @UIP
        , mkValEntry "lineCapSquare" "Use a square line cap" $ lineCapSquare @UIP
        , mkValEntry "lineWidth" "Use this width for line" $ lineWidth @UIP
        , mkValEntry "operatorOver" "Draw over" $ operatorOver @UIP
        , mkValEntry "setSourceColour" "Set the source colour" setSourceColour
        , docTreeEntry
              "Path"
              ""
              [ mkTypeEntry "Path" "A path on a drawing." $ MkBoundType pathGroundType
              , mkSubtypeRelationEntry "[Path]" "Path" "Monoidal relationship" $
                pure $ monoidSubypeConversionEntry pathGroundType
              , mkValEntry "closePath" "close the path into a loop" closePath
              , mkValEntry "arc" "arc" arc
              , mkValEntry "arcNegative" "arc negative" arcNegative
              , mkValEntry "lineTo" "draw a line to this point" lineTo
              , mkValEntry "moveTo" "move to this point" moveTo
              ]
        , docTreeEntry
              "Actions"
              ""
              [ mkValEntry "onClick" "Action to perform on click" $ onClick . runPinaforeAction
              , mkValEntry "ifPoint" "Restrict actions based on point" $ ifPoint @UIEvents
              ]
        , docTreeEntry
              "Transform"
              ""
              [ mkValEntry "translate" "Translate a drawing" $ translate @UIP
              , mkValEntry "rotate" "Rotate a drawing" $ rotate @UIP
              , mkValEntry "scale" "Scale a drawing" $ scale @UIP
              ]
        ]
