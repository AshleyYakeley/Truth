{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.Drawing
    ( drawingLibraryModule
    , LangDrawing
    ) where

import Changes.UI.GTK
import Data.Shim
import Graphics.Cairo.Functional
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Colour
import Shapes hiding (rotate)
import Shapes.Numeric

-- LangDrawing
type LangDrawing = UIDrawing

drawingGroundType :: PinaforeGroundType '[] LangDrawing
drawingGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangDrawing)|]) "Drawing"

instance Is PolarityType polarity => HasPinaforeType polarity LangDrawing where
    pinaforeType = groundPinaforeType

instance HasPinaforeGroundType '[] LangDrawing where
    pinaforeGroundType = drawingGroundType

-- LangPath
type LangPath = Path

pathGroundType :: PinaforeGroundType '[] LangPath
pathGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangPath)|]) "Path"

instance HasPinaforeGroundType '[] LangPath where
    pinaforeGroundType = pathGroundType

-- LangPattern
type LangPattern = Pattern

patternGroundType :: PinaforeGroundType '[] LangPattern
patternGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangPattern)|]) "Pattern"

instance HasPinaforeGroundType '[] LangPattern where
    pinaforeGroundType = patternGroundType

type UIP = PixelPoint -> UIEvents

colourToTuple :: LangColour -> (Double, Double, Double)
colourToTuple (MkSRGBColour r g b) = (r, g, b)

alphaColourToTuple :: LangAlphaColour -> ((Double, Double, Double), Double)
alphaColourToTuple (MkLangAlphaColour op col) = (colourToTuple col, op)

toPatternColorStop :: (Double, LangAlphaColour) -> PatternColorStop
toPatternColorStop (offset, MkLangAlphaColour op col) = MkPatternColorStop offset (colourToTuple col, Just op)

instance HasPinaforeType 'Negative PatternColorStop where
    pinaforeType = mapNegShimWit (functionToShim "toPatternColorStop" toPatternColorStop) pinaforeType

source :: LangAlphaColour -> LangDrawing -> LangDrawing
source acol = sourceRGBA $ alphaColourToTuple acol

solidPattern :: LangAlphaColour -> LangPattern
solidPattern acol = rgbaPattern $ alphaColourToTuple acol

fontFace' :: Text -> Bool -> Bool -> Drawing a -> Drawing a
fontFace' fname italic bold =
    fontFace
        fname
        (if italic
             then FontSlantItalic
             else FontSlantNormal)
        (if bold
             then FontWeightBold
             else FontWeightNormal)

drawingLibraryModule :: LibraryModule
drawingLibraryModule =
    MkDocTree
        "Drawing"
        ""
        [ mkTypeEntry "Drawing" "Something that can be drawn." $ MkBoundType drawingGroundType
        , monoidSubtypeRelationEntry @LangDrawing
        , docTreeEntry
              "Actions"
              ""
              [ mkValEntry "onClick" "Action to perform on click" $ onClick . runPinaforeAction
              , mkValEntry "ifPoint" "Restrict actions based on point" $ ifPoint @UIEvents
              , mkValEntry "ifInRect" "Restrict actions to within a rectangle, as ((left,top),(width,height))" $
                ifInRect @UIEvents
              ]
        , docTreeEntry
              "Transformation"
              ""
              [ mkValEntry "translate" "Translate a drawing" $ translate @UIP
              , mkValEntry "rotate" "Rotate a drawing" $ rotate @UIP
              , mkValEntry "scale" "Scale a drawing" $ scale @UIP
              ]
        , docTreeEntry
              "Painting"
              ""
              [ mkValEntry "paint" "Paint everywhere (within the clip)" $ paint @UIP
              , mkValEntry "paintAlpha" "Paint everywhere with this alpha (within the clip)" $ paintWithAlpha @UIP
              , mkValEntry "source" "Set the source colour" source
              , mkValEntry "operatorClear" "" $ operatorClear @UIP
              , mkValEntry "operatorSource" "" $ operatorSource @UIP
              , mkValEntry "operatorOver" "" $ operatorOver @UIP
              , mkValEntry "operatorIn" "" $ operatorIn @UIP
              , mkValEntry "operatorOut" "" $ operatorOut @UIP
              , mkValEntry "operatorAtop" "" $ operatorAtop @UIP
              , mkValEntry "operatorDest" "" $ operatorDest @UIP
              , mkValEntry "operatorDestOver" "" $ operatorDestOver @UIP
              , mkValEntry "operatorDestIn" "" $ operatorDestIn @UIP
              , mkValEntry "operatorDestOut" "" $ operatorDestOut @UIP
              , mkValEntry "operatorDestAtop" "" $ operatorDestAtop @UIP
              , mkValEntry "operatorXor" "" $ operatorXor @UIP
              , mkValEntry "operatorAdd" "" $ operatorAdd @UIP
              , mkValEntry "operatorSaturate" "" $ operatorSaturate @UIP
              , mkValEntry "operatorMultiply" "" $ operatorMultiply @UIP
              , mkValEntry "operatorScreen" "" $ operatorScreen @UIP
              , mkValEntry "operatorOverlay" "" $ operatorOverlay @UIP
              , mkValEntry "operatorDarken" "" $ operatorDarken @UIP
              , mkValEntry "operatorLighten" "" $ operatorLighten @UIP
              , mkValEntry "operatorColorDodge" "" $ operatorColorDodge @UIP
              , mkValEntry "operatorColorBurn" "" $ operatorColorBurn @UIP
              , mkValEntry "operatorHardLight" "" $ operatorHardLight @UIP
              , mkValEntry "operatorSoftLight" "" $ operatorSoftLight @UIP
              , mkValEntry "operatorDifference" "" $ operatorDifference @UIP
              , mkValEntry "operatorExclusion" "" $ operatorExclusion @UIP
              , mkValEntry "operatorHslHue" "" $ operatorHslHue @UIP
              , mkValEntry "operatorHslSaturation" "" $ operatorHslSaturation @UIP
              , mkValEntry "operatorHslColor" "" $ operatorHslColor @UIP
              , mkValEntry "operatorHslLuminosity" "" $ operatorHslLuminosity @UIP
              ]
        , docTreeEntry
              "Path"
              ""
              [ mkTypeEntry "Path" "A path on a drawing." $ MkBoundType pathGroundType
              , monoidSubtypeRelationEntry @LangPath
              , mkValEntry "stroke" "Draw this path" $ stroke @UIP
              , mkValEntry "fill" "Fill this path" $ fill @UIP
              , mkValEntry "clip" "Clip drawing to this path" $ clip @UIP
              , docTreeEntry
                    "Construction"
                    ""
                    [ mkValEntry "closePath" "close the path into a loop" closePath
                    , mkValEntry "moveTo" "move to this point" moveTo
                    , mkValEntry "lineTo" "draw a line to this point" lineTo
                    , mkValEntry "curveTo" "draw a curve to this point" curveTo
                    , mkValEntry "relMoveTo" "move by this displacement" relMoveTo
                    , mkValEntry "relLineTo" "draw a line by this displacement" relLineTo
                    , mkValEntry "relCurveTo" "draw a curve by this displacement" relCurveTo
                    , mkValEntry "rectangle" "draw a rectangle" rectangle
                    , mkValEntry "arc" "`arc center radius angle1 angle2`" arc
                    , mkValEntry "arcNegative" "`arcNegative center radius angle1 angle2`" arcNegative
                    ]
              , docTreeEntry
                    "Properties"
                    ""
                    [ mkValEntry "lineWidth" "Use this width for line" $ lineWidth @UIP
                    , mkValEntry "lineJoinMitre" "Use a mitred line join, with limit" $ lineJoinMitre @UIP
                    , mkValEntry "lineJoinRound" "Use a round line join" $ lineJoinRound @UIP
                    , mkValEntry "lineJoinBevel" "Use a bevel line join" $ lineJoinBevel @UIP
                    , mkValEntry "lineCapButt" "Use a butt line cap" $ lineCapButt @UIP
                    , mkValEntry "lineCapRound" "Use a round line cap" $ lineCapRound @UIP
                    , mkValEntry "lineCapSquare" "Use a square line cap" $ lineCapSquare @UIP
                    , mkValEntry "dash" "Use a dash pattern for line" $ dash @UIP
                    , mkValEntry "fillRuleNonZero" "fill for non-zero winding number" $ fillRuleWinding @UIP
                    , mkValEntry "fillRuleOdd" "fill for odd winding number" $ fillRuleEvenOdd @UIP
                    ]
              ]
        , docTreeEntry
              "Text"
              ""
              [ mkValEntry "textPath" "" $ textPath
              , mkValEntry "fontFace" "" $ fontFace' @UIP
              , mkValEntry "fontSize" "" $ fontSize @UIP
              ]
        , docTreeEntry
              "Patterns"
              ""
              [ mkTypeEntry "Pattern" "" $ MkBoundType patternGroundType
              , mkValEntry "patternSource" "" $ patternSource @UIP
              , mkValEntry "patternMask" "" $ patternMask @UIP
              , mkValEntry "solidPattern" "" solidPattern
              , mkValEntry "linearPattern" "" $ linearPattern
              , mkValEntry "radialPattern" "" $ radialPattern
              , docTreeEntry
                    "From Drawings"
                    ""
                    [ mkValEntry "colorDrawingPattern" "" $ colorDrawingPattern @UIP
                    , mkValEntry "alphaDrawingPattern" "" $ alphaDrawingPattern @UIP
                    , mkValEntry "colorAlphaDrawingPattern" "" $ colorAlphaDrawingPattern @UIP
                    ]
              ]
        ]
