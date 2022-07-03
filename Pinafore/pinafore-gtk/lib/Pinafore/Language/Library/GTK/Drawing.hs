{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.Drawing
    ( drawingLibraryModule
    , LangDrawing(..)
    ) where

import Changes.Core
import Changes.UI.GTK
import Data.Shim
import Graphics.Cairo.Functional
import Pinafore.Base
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Colour
import Pinafore.Language.Library.GTK.Context
import Shapes hiding (rotate)
import Shapes.Numeric

-- LangDrawing
newtype LangDrawing = MkLangDrawing
    { unLangDrawing :: (View --> IO) -> UIDrawing
    } deriving (Semigroup, Monoid)

liftDrawing :: UIDrawing -> LangDrawing
liftDrawing d = MkLangDrawing $ \_ -> d

lift1Drawing :: (a -> UIDrawing) -> a -> LangDrawing
lift1Drawing d a = liftDrawing $ d a

hoistDrawing :: (UIDrawing -> UIDrawing) -> LangDrawing -> LangDrawing
hoistDrawing f (MkLangDrawing ld) = MkLangDrawing $ \c -> f $ ld c

hoist1Drawing :: (a -> UIDrawing -> UIDrawing) -> a -> LangDrawing -> LangDrawing
hoist1Drawing f a = hoistDrawing $ f a

hoist2Drawing :: (a -> b -> UIDrawing -> UIDrawing) -> a -> b -> LangDrawing -> LangDrawing
hoist2Drawing f a = hoist1Drawing $ f a

hoist3Drawing :: (a -> b -> c -> UIDrawing -> UIDrawing) -> a -> b -> c -> LangDrawing -> LangDrawing
hoist3Drawing f a = hoist2Drawing $ f a

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
newtype LangPattern =
    MkLangPattern ((View --> IO) -> Pattern)

liftPattern :: Pattern -> LangPattern
liftPattern p = MkLangPattern $ \_ -> p

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
source acol = hoistDrawing $ sourceRGBA $ alphaColourToTuple acol

solidPattern :: LangAlphaColour -> LangPattern
solidPattern acol = MkLangPattern $ \_ -> rgbaPattern $ alphaColourToTuple acol

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

langOnClick :: (?pinafore :: PinaforeContext) => PinaforeAction () -> LangDrawing
langOnClick action = MkLangDrawing $ \unlift -> onClick $ gvRunAction unlift action

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
              [ mkValEntry "onClick" "Action to perform on click" langOnClick
              , mkValEntry "ifPoint" "Restrict actions based on point" $ hoist1Drawing $ ifPoint @UIEvents
              , mkValEntry "ifInRect" "Restrict actions to within a rectangle, as ((left,top),(width,height))" $
                hoist1Drawing $ ifInRect @UIEvents
              ]
        , docTreeEntry
              "Transformation"
              ""
              [ mkValEntry "translate" "Translate a drawing" $ hoist1Drawing $ translate @UIP
              , mkValEntry "rotate" "Rotate a drawing" $ hoist1Drawing $ rotate @UIP
              , mkValEntry "scale" "Scale a drawing" $ hoist1Drawing $ scale @UIP
              ]
        , docTreeEntry
              "Painting"
              ""
              [ mkValEntry "paint" "Paint everywhere (within the clip)" $ liftDrawing $ paint @UIP
              , mkValEntry "paintAlpha" "Paint everywhere with this alpha (within the clip)" $
                lift1Drawing $ paintWithAlpha @UIP
              , mkValEntry "source" "Set the source colour" source
              , mkValEntry "operatorClear" "" $ hoistDrawing $ operatorClear @UIP
              , mkValEntry "operatorSource" "" $ hoistDrawing $ operatorSource @UIP
              , mkValEntry "operatorOver" "" $ hoistDrawing $ operatorOver @UIP
              , mkValEntry "operatorIn" "" $ hoistDrawing $ operatorIn @UIP
              , mkValEntry "operatorOut" "" $ hoistDrawing $ operatorOut @UIP
              , mkValEntry "operatorAtop" "" $ hoistDrawing $ operatorAtop @UIP
              , mkValEntry "operatorDest" "" $ hoistDrawing $ operatorDest @UIP
              , mkValEntry "operatorDestOver" "" $ hoistDrawing $ operatorDestOver @UIP
              , mkValEntry "operatorDestIn" "" $ hoistDrawing $ operatorDestIn @UIP
              , mkValEntry "operatorDestOut" "" $ hoistDrawing $ operatorDestOut @UIP
              , mkValEntry "operatorDestAtop" "" $ hoistDrawing $ operatorDestAtop @UIP
              , mkValEntry "operatorXor" "" $ hoistDrawing $ operatorXor @UIP
              , mkValEntry "operatorAdd" "" $ hoistDrawing $ operatorAdd @UIP
              , mkValEntry "operatorSaturate" "" $ hoistDrawing $ operatorSaturate @UIP
              , mkValEntry "operatorMultiply" "" $ hoistDrawing $ operatorMultiply @UIP
              , mkValEntry "operatorScreen" "" $ hoistDrawing $ operatorScreen @UIP
              , mkValEntry "operatorOverlay" "" $ hoistDrawing $ operatorOverlay @UIP
              , mkValEntry "operatorDarken" "" $ hoistDrawing $ operatorDarken @UIP
              , mkValEntry "operatorLighten" "" $ hoistDrawing $ operatorLighten @UIP
              , mkValEntry "operatorColorDodge" "" $ hoistDrawing $ operatorColorDodge @UIP
              , mkValEntry "operatorColorBurn" "" $ hoistDrawing $ operatorColorBurn @UIP
              , mkValEntry "operatorHardLight" "" $ hoistDrawing $ operatorHardLight @UIP
              , mkValEntry "operatorSoftLight" "" $ hoistDrawing $ operatorSoftLight @UIP
              , mkValEntry "operatorDifference" "" $ hoistDrawing $ operatorDifference @UIP
              , mkValEntry "operatorExclusion" "" $ hoistDrawing $ operatorExclusion @UIP
              , mkValEntry "operatorHslHue" "" $ hoistDrawing $ operatorHslHue @UIP
              , mkValEntry "operatorHslSaturation" "" $ hoistDrawing $ operatorHslSaturation @UIP
              , mkValEntry "operatorHslColor" "" $ hoistDrawing $ operatorHslColor @UIP
              , mkValEntry "operatorHslLuminosity" "" $ hoistDrawing $ operatorHslLuminosity @UIP
              ]
        , docTreeEntry
              "Path"
              ""
              [ mkTypeEntry "Path" "A path on a drawing." $ MkBoundType pathGroundType
              , monoidSubtypeRelationEntry @LangPath
              , mkValEntry "stroke" "Draw this path" $ lift1Drawing $ stroke @UIP
              , mkValEntry "fill" "Fill this path" $ lift1Drawing $ fill @UIP
              , mkValEntry "clip" "Clip drawing to this path" $ hoist1Drawing $ clip @UIP
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
                    [ mkValEntry "lineWidth" "Use this width for line" $ hoist1Drawing $ lineWidth @UIP
                    , mkValEntry "lineJoinMitre" "Use a mitred line join, with limit" $
                      hoist1Drawing $ lineJoinMitre @UIP
                    , mkValEntry "lineJoinRound" "Use a round line join" $ hoistDrawing $ lineJoinRound @UIP
                    , mkValEntry "lineJoinBevel" "Use a bevel line join" $ hoistDrawing $ lineJoinBevel @UIP
                    , mkValEntry "lineCapButt" "Use a butt line cap" $ hoistDrawing $ lineCapButt @UIP
                    , mkValEntry "lineCapRound" "Use a round line cap" $ hoistDrawing $ lineCapRound @UIP
                    , mkValEntry "lineCapSquare" "Use a square line cap" $ hoistDrawing $ lineCapSquare @UIP
                    , mkValEntry "dash" "Use a dash pattern for line" $ hoist2Drawing $ dash @UIP
                    , mkValEntry "fillRuleNonZero" "fill for non-zero winding number" $
                      hoistDrawing $ fillRuleWinding @UIP
                    , mkValEntry "fillRuleOdd" "fill for odd winding number" $ hoistDrawing $ fillRuleEvenOdd @UIP
                    ]
              ]
        , docTreeEntry
              "Text"
              ""
              [ mkValEntry "textPath" "" $ textPath
              , mkValEntry "fontFace" "" $ hoist3Drawing $ fontFace' @UIP
              , mkValEntry "fontSize" "" $ hoist1Drawing $ fontSize @UIP
              ]
        , docTreeEntry
              "Patterns"
              ""
              [ mkTypeEntry "Pattern" "" $ MkBoundType patternGroundType
              , mkValEntry "patternSource" "" $ \(MkLangPattern fpat) (MkLangDrawing fd) ->
                    MkLangDrawing $ \c -> patternSource @UIP (fpat c) (fd c)
              , mkValEntry "patternMask" "" $ \(MkLangPattern fpat) -> MkLangDrawing $ \c -> patternMask @UIP $ fpat c
              , mkValEntry "solidPattern" "" solidPattern
              , mkValEntry "linearPattern" "" $ \a b c -> liftPattern $ linearPattern a b c
              , mkValEntry "radialPattern" "" $ \a b c d e -> liftPattern $ radialPattern a b c d e
              , docTreeEntry
                    "From Drawings"
                    ""
                    [ mkValEntry "colorDrawingPattern" "" $ \(MkLangDrawing fd) ->
                          MkLangPattern $ \c -> colorDrawingPattern @UIP $ fd c
                    , mkValEntry "alphaDrawingPattern" "" $ \(MkLangDrawing fd) ->
                          MkLangPattern $ \c -> alphaDrawingPattern @UIP $ fd c
                    , mkValEntry "colorAlphaDrawingPattern" "" $ \(MkLangDrawing fd) ->
                          MkLangPattern $ \c -> colorAlphaDrawingPattern @UIP $ fd c
                    ]
              ]
        ]
