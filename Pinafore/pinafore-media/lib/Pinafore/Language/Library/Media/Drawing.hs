{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Media.Drawing
    ( drawingLibraryModule
    , LangDrawing(..)
    ) where

import Data.Shim
import Graphics.Cairo.Functional
import Pinafore.Language.API
import Pinafore.Language.Library.Media.Colour
import Shapes hiding (rotate)
import Shapes.Numeric

-- LangDrawing
newtype LangDrawing a = MkLangDrawing
    { unLangDrawing :: Drawing (PixelPoint -> a)
    } deriving (Semigroup, Monoid)

instance Functor LangDrawing where
    fmap ab (MkLangDrawing d) = MkLangDrawing $ fmap (fmap ab) d

instance Applicative LangDrawing where
    pure a = MkLangDrawing $ pure $ pure a
    liftA2 f (MkLangDrawing pa) (MkLangDrawing pb) = MkLangDrawing $ liftA2 (liftA2 f) pa pb

instance RepresentationalRole LangDrawing where
    representationalCoercion MkCoercion = MkCoercion

instance MaybeRepresentational LangDrawing where
    maybeRepresentational = Just Dict

instance HasVariance LangDrawing where
    type VarianceOf LangDrawing = 'Covariance

drawingGroundType :: PinaforeGroundType '[ CoCCRVariance] LangDrawing
drawingGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangDrawing)|]) "Drawing"

instance HasPinaforeGroundType '[ CoCCRVariance] LangDrawing where
    pinaforeGroundType = drawingGroundType

langPointDrawing :: LangDrawing (Double, Double)
langPointDrawing = MkLangDrawing $ pointDrawing id

liftDrawing :: forall a. Drawing a -> LangDrawing a
liftDrawing d = MkLangDrawing $ fmap (\a _ -> a) d

lift1Drawing :: forall a b. (a -> Drawing b) -> a -> LangDrawing b
lift1Drawing d a = liftDrawing $ d a

hoistDrawing :: forall t. (Drawing --> Drawing) -> LangDrawing t -> LangDrawing t
hoistDrawing f (MkLangDrawing ld) = MkLangDrawing $ f ld

hoist1Drawing :: forall t a. (a -> Drawing --> Drawing) -> a -> LangDrawing t -> LangDrawing t
hoist1Drawing f a = hoistDrawing $ f a

hoist2Drawing :: forall t a b. (a -> b -> Drawing --> Drawing) -> a -> b -> LangDrawing t -> LangDrawing t
hoist2Drawing f a = hoist1Drawing $ f a

hoist3Drawing :: forall t a b c. (a -> b -> c -> Drawing --> Drawing) -> a -> b -> c -> LangDrawing t -> LangDrawing t
hoist3Drawing f a = hoist2Drawing $ f a

langIfPoint :: ((Double, Double) -> Bool) -> LangDrawing A -> LangDrawing [A]
langIfPoint f (MkLangDrawing d) = MkLangDrawing $ ifPoint f (fmap (fmap pure) d)

langIfInRect :: ((Double, Double), (Double, Double)) -> LangDrawing A -> LangDrawing [A]
langIfInRect ((x0, y0), (w, h)) = langIfPoint $ \(x, y) -> (x >= x0) && (x < x0 + w) && (y >= y0) && (y < y0 + h)

-- LangPath
type LangPath = Path

pathGroundType :: PinaforeGroundType '[] LangPath
pathGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangPath)|]) "Path"

instance HasPinaforeGroundType '[] LangPath where
    pinaforeGroundType = pathGroundType

-- LangPattern
newtype LangPattern =
    MkLangPattern Pattern

liftPattern :: Pattern -> LangPattern
liftPattern p = MkLangPattern p

patternGroundType :: PinaforeGroundType '[] LangPattern
patternGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangPattern)|]) "Pattern"

instance HasPinaforeGroundType '[] LangPattern where
    pinaforeGroundType = patternGroundType

colourToTuple :: LangColour -> (Double, Double, Double)
colourToTuple (MkSRGBColour r g b) = (r, g, b)

alphaColourToTuple :: LangAlphaColour -> ((Double, Double, Double), Double)
alphaColourToTuple (MkLangAlphaColour op col) = (colourToTuple col, op)

toPatternColorStop :: (Double, LangAlphaColour) -> PatternColorStop
toPatternColorStop (offset, MkLangAlphaColour op col) = MkPatternColorStop offset (colourToTuple col, Just op)

instance HasPinaforeType 'Negative PatternColorStop where
    pinaforeType = mapNegShimWit (functionToShim "toPatternColorStop" toPatternColorStop) pinaforeType

langSource :: LangAlphaColour -> LangDrawing A -> LangDrawing A
langSource acol = hoistDrawing $ sourceRGBA $ alphaColourToTuple acol

solidPattern :: LangAlphaColour -> LangPattern
solidPattern acol = MkLangPattern $ rgbaPattern $ alphaColourToTuple acol

fontFace' :: Text -> Bool -> Bool -> Drawing --> Drawing
fontFace' fname italic bold =
    fontFace
        fname
        (if italic
             then FontSlantItalic
             else FontSlantNormal)
        (if bold
             then FontWeightBold
             else FontWeightNormal)

{-
langOnClick :: (?pinafore :: PinaforeContext) => PinaforeAction () -> LangDrawing
langOnClick action = MkLangDrawing $ \unlift -> onClick $ gvRunAction unlift action
-}
drawingLibraryModule :: LibraryModule
drawingLibraryModule =
    MkDocTree
        "Drawing"
        ""
        [ mkTypeEntry "Drawing" "Something that can be drawn." $ MkBoundType drawingGroundType
        , hasSubtypeRelationEntry @[LangDrawing A] @(LangDrawing [A]) "Monoidal relationship" $
          functionToShim "mconcat" $ \dd -> mconcat $ fmap (fmap pure) dd
        , mkValEntry "pureDrawing" "" $ pure @LangDrawing @A
        , mkValEntry "mapDrawing" "" $ fmap @LangDrawing @A @B
        , mkValEntry "apDrawing" "" $ (<*>) @LangDrawing @A @B
        , mkValEntry "position" "" langPointDrawing
        , mkValEntry "ifPoint" "Restrict actions based on point" langIfPoint
        , mkValEntry "ifInRect" "Restrict actions to within a rectangle, as ((left,top),(width,height))" langIfInRect
        , docTreeEntry
              "Transformation"
              ""
              [ mkValEntry "translate" "Translate a drawing" $ hoist1Drawing @A translate
              , mkValEntry "rotate" "Rotate a drawing" $ hoist1Drawing @A rotate
              , mkValEntry "scale" "Scale a drawing" $ hoist1Drawing @A scale
              ]
        , docTreeEntry
              "Painting"
              ""
              [ mkValEntry "paint" "Paint everywhere (within the clip)" $ liftDrawing $ paint @[BottomType]
              , mkValEntry "paintAlpha" "Paint everywhere with this alpha (within the clip)" $
                lift1Drawing $ paintWithAlpha @[BottomType]
              , mkValEntry "source" "Set the source colour" langSource
              , mkValEntry "operatorClear" "" $ hoistDrawing @A operatorClear
              , mkValEntry "operatorSource" "" $ hoistDrawing @A operatorSource
              , mkValEntry "operatorOver" "" $ hoistDrawing @A operatorOver
              , mkValEntry "operatorIn" "" $ hoistDrawing @A operatorIn
              , mkValEntry "operatorOut" "" $ hoistDrawing @A operatorOut
              , mkValEntry "operatorAtop" "" $ hoistDrawing @A operatorAtop
              , mkValEntry "operatorDest" "" $ hoistDrawing @A operatorDest
              , mkValEntry "operatorDestOver" "" $ hoistDrawing @A operatorDestOver
              , mkValEntry "operatorDestIn" "" $ hoistDrawing @A operatorDestIn
              , mkValEntry "operatorDestOut" "" $ hoistDrawing @A operatorDestOut
              , mkValEntry "operatorDestAtop" "" $ hoistDrawing @A operatorDestAtop
              , mkValEntry "operatorXor" "" $ hoistDrawing @A operatorXor
              , mkValEntry "operatorAdd" "" $ hoistDrawing @A operatorAdd
              , mkValEntry "operatorSaturate" "" $ hoistDrawing @A operatorSaturate
              , mkValEntry "operatorMultiply" "" $ hoistDrawing @A operatorMultiply
              , mkValEntry "operatorScreen" "" $ hoistDrawing @A operatorScreen
              , mkValEntry "operatorOverlay" "" $ hoistDrawing @A operatorOverlay
              , mkValEntry "operatorDarken" "" $ hoistDrawing @A operatorDarken
              , mkValEntry "operatorLighten" "" $ hoistDrawing @A operatorLighten
              , mkValEntry "operatorColorDodge" "" $ hoistDrawing @A operatorColorDodge
              , mkValEntry "operatorColorBurn" "" $ hoistDrawing @A operatorColorBurn
              , mkValEntry "operatorHardLight" "" $ hoistDrawing @A operatorHardLight
              , mkValEntry "operatorSoftLight" "" $ hoistDrawing @A operatorSoftLight
              , mkValEntry "operatorDifference" "" $ hoistDrawing @A operatorDifference
              , mkValEntry "operatorExclusion" "" $ hoistDrawing @A operatorExclusion
              , mkValEntry "operatorHslHue" "" $ hoistDrawing @A operatorHslHue
              , mkValEntry "operatorHslSaturation" "" $ hoistDrawing @A operatorHslSaturation
              , mkValEntry "operatorHslColor" "" $ hoistDrawing @A operatorHslColor
              , mkValEntry "operatorHslLuminosity" "" $ hoistDrawing @A operatorHslLuminosity
              ]
        , docTreeEntry
              "Path"
              ""
              [ mkTypeEntry "Path" "A path on a drawing." $ MkBoundType pathGroundType
              , monoidSubtypeRelationEntry @LangPath
              , mkValEntry "stroke" "Draw this path" $ lift1Drawing $ stroke @[BottomType]
              , mkValEntry "fill" "Fill this path" $ lift1Drawing $ fill @[BottomType]
              , mkValEntry "clip" "Clip drawing to this path" $ hoist1Drawing @A clip
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
                    [ mkValEntry "lineWidth" "Use this width for line" $ hoist1Drawing @A $ lineWidth
                    , mkValEntry "lineJoinMitre" "Use a mitred line join, with limit" $ hoist1Drawing @A $ lineJoinMitre
                    , mkValEntry "lineJoinRound" "Use a round line join" $ hoistDrawing @A $ lineJoinRound
                    , mkValEntry "lineJoinBevel" "Use a bevel line join" $ hoistDrawing @A $ lineJoinBevel
                    , mkValEntry "lineCapButt" "Use a butt line cap" $ hoistDrawing @A $ lineCapButt
                    , mkValEntry "lineCapRound" "Use a round line cap" $ hoistDrawing @A $ lineCapRound
                    , mkValEntry "lineCapSquare" "Use a square line cap" $ hoistDrawing @A $ lineCapSquare
                    , mkValEntry "dash" "Use a dash pattern for line" $ hoist2Drawing @A $ dash
                    , mkValEntry "fillRuleNonZero" "fill for non-zero winding number" $
                      hoistDrawing @A $ fillRuleWinding
                    , mkValEntry "fillRuleOdd" "fill for odd winding number" $ hoistDrawing @A $ fillRuleEvenOdd
                    ]
              ]
        , docTreeEntry
              "Text"
              ""
              [ mkValEntry "textPath" "" $ textPath
              , mkValEntry "fontFace" "" $ hoist3Drawing @A fontFace'
              , mkValEntry "fontSize" "" $ hoist1Drawing @A fontSize
              ]
        , docTreeEntry
              "Patterns"
              ""
              [ mkTypeEntry "Pattern" "" $ MkBoundType patternGroundType
              , mkValEntry "patternSource" "" langPatternSource
              , mkValEntry "patternMask" "" langPatternMask
              , mkValEntry "solidPattern" "" solidPattern
              , mkValEntry "linearPattern" "" $ \a b c -> liftPattern $ linearPattern a b c
              , mkValEntry "radialPattern" "" $ \a b c d e -> liftPattern $ radialPattern a b c d e
              , docTreeEntry
                    "From Drawings"
                    ""
                    [ mkValEntry "colorDrawingPattern" "" langColorDrawingPattern
                    , mkValEntry "alphaDrawingPattern" "" langAlphaDrawingPattern
                    , mkValEntry "colorAlphaDrawingPattern" "" langColorAlphaDrawingPattern
                    ]
              ]
        ]

langPatternSource :: LangPattern -> LangDrawing A -> LangDrawing A
langPatternSource (MkLangPattern pat) (MkLangDrawing d) = MkLangDrawing $ patternSource pat d

langPatternMask :: LangPattern -> LangDrawing [BottomType]
langPatternMask (MkLangPattern pat) = MkLangDrawing $ patternMask pat

langColorDrawingPattern :: LangDrawing TopType -> LangPattern
langColorDrawingPattern (MkLangDrawing d) = MkLangPattern $ colorDrawingPattern d

langAlphaDrawingPattern :: LangDrawing TopType -> LangPattern
langAlphaDrawingPattern (MkLangDrawing d) = MkLangPattern $ alphaDrawingPattern d

langColorAlphaDrawingPattern :: LangDrawing TopType -> LangPattern
langColorAlphaDrawingPattern (MkLangDrawing d) = MkLangPattern $ colorAlphaDrawingPattern d
