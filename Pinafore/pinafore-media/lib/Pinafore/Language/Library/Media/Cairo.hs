{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Media.Cairo
    ( cairoStuff
    , LangDrawing(..)
    ) where

import Data.Media.Image
import Data.Shim
import Graphics.Cairo.Functional
import Graphics.Cairo.Image
import Pinafore.Language.API
import Pinafore.Language.Library.Media.Colour
import Pinafore.Language.Library.Media.Image
import Shapes hiding (rotate)
import Shapes.Numeric

-- LangDrawing
newtype LangDrawing a = MkLangDrawing
    { unLangDrawing :: Drawing (PixelPoint -> [a])
    } deriving (Semigroup, Monoid)

instance Functor LangDrawing where
    fmap ab (MkLangDrawing d) = MkLangDrawing $ fmap (fmap (fmap ab)) d

instance Applicative LangDrawing where
    pure a = MkLangDrawing $ pure $ pure $ pure a
    liftA2 f (MkLangDrawing pa) (MkLangDrawing pb) = MkLangDrawing $ liftA2 (liftA2 (liftA2 f)) pa pb

instance RepresentationalRole LangDrawing where
    representationalCoercion MkCoercion = MkCoercion

instance MaybeRepresentational LangDrawing where
    maybeRepresentational = Just Dict

instance HasVariance LangDrawing where
    type VarianceOf LangDrawing = 'Covariance

drawingGroundType :: QGroundType '[ CoCCRVariance] LangDrawing
drawingGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangDrawing)|]) "Drawing"

instance HasQGroundType '[ CoCCRVariance] LangDrawing where
    qGroundType = drawingGroundType

langPointDrawing :: LangDrawing (Double, Double)
langPointDrawing = MkLangDrawing $ pointDrawing pure

liftDrawing :: forall a. Drawing [a] -> LangDrawing a
liftDrawing d = MkLangDrawing $ fmap (\a _ -> a) d

lift1Drawing :: forall a b. (a -> Drawing [b]) -> a -> LangDrawing b
lift1Drawing d a = liftDrawing $ d a

hoistDrawing :: forall t. (Drawing --> Drawing) -> LangDrawing t -> LangDrawing t
hoistDrawing f (MkLangDrawing ld) = MkLangDrawing $ f ld

hoist1Drawing :: forall t a. (a -> Drawing --> Drawing) -> a -> LangDrawing t -> LangDrawing t
hoist1Drawing f a = hoistDrawing $ f a

hoist2Drawing :: forall t a b. (a -> b -> Drawing --> Drawing) -> a -> b -> LangDrawing t -> LangDrawing t
hoist2Drawing f a = hoist1Drawing $ f a

hoist3Drawing :: forall t a b c. (a -> b -> c -> Drawing --> Drawing) -> a -> b -> c -> LangDrawing t -> LangDrawing t
hoist3Drawing f a = hoist2Drawing $ f a

langIfPoint :: ((Double, Double) -> Bool) -> LangDrawing A -> LangDrawing A
langIfPoint f (MkLangDrawing d) = MkLangDrawing $ ifPoint f d

langIfInRect :: ((Double, Double), (Double, Double)) -> LangDrawing A -> LangDrawing A
langIfInRect ((x0, y0), (w, h)) = langIfPoint $ \(x, y) -> (x >= x0) && (x < x0 + w) && (y >= y0) && (y < y0 + h)

-- LangPath
type LangPath = Path

pathGroundType :: QGroundType '[] LangPath
pathGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangPath)|]) "Path"

instance HasQGroundType '[] LangPath where
    qGroundType = pathGroundType

-- LangPattern
newtype LangPattern =
    MkLangPattern Pattern

liftPattern :: Pattern -> LangPattern
liftPattern p = MkLangPattern p

patternGroundType :: QGroundType '[] LangPattern
patternGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangPattern)|]) "Pattern"

instance HasQGroundType '[] LangPattern where
    qGroundType = patternGroundType

colourToTuple :: LangColour -> (Double, Double, Double)
colourToTuple (MkPerceptualSRGBFraction r g b) = (r, g, b)

alphaColourToTuple :: LangAlphaColour -> ((Double, Double, Double), Double)
alphaColourToTuple (MkAlphaColourFraction op col) = (colourToTuple col, op)

toPatternColorStop :: (Double, LangAlphaColour) -> PatternColorStop
toPatternColorStop (offset, MkAlphaColourFraction op col) = MkPatternColorStop offset (colourToTuple col, Just op)

instance HasQType 'Negative PatternColorStop where
    qType = mapNegShimWit (functionToShim "toPatternColorStop" toPatternColorStop) qType

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

drawToImage :: (Int, Int) -> LangDrawing a -> LangImage
drawToImage s (MkLangDrawing d) = MkLangImage $ MkSomeFor RGBA8PixelType $ renderToImage s $ drawingRender d

cairoStuff :: BindDocTree ()
cairoStuff =
    headingBDT "Cairo" "" $
    pure $
    namespaceBDT
        "Cairo"
        ""
        [ headingBDT
              "Drawing"
              ""
              [ typeBDT "Drawing" "Something that can be drawn." (MkSomeGroundType drawingGroundType) []
              , namespaceBDT "Drawing" "" $
                monoidEntries @_ @(LangDrawing A) <>
                applicativeEntries @_ @LangDrawing <>
                [ valBDT "map" "" $ fmap @LangDrawing @A @B
                , valBDT "toImage" "" $ drawToImage @TopType
                , headingBDT
                      "Position"
                      ""
                      [ valBDT "position" "" langPointDrawing
                      , valBDT "ifPoint" "Restrict actions based on point" langIfPoint
                      , valBDT
                            "ifInRect"
                            "Restrict actions to within a rectangle, as ((left,top),(width,height))"
                            langIfInRect
                      ]
                , headingBDT
                      "Transformation"
                      ""
                      [ valBDT "translate" "Translate a drawing" $ hoist1Drawing @A translate
                      , valBDT "rotate" "Rotate a drawing" $ hoist1Drawing @A rotate
                      , valBDT "scale" "Scale a drawing" $ hoist1Drawing @A scale
                      ]
                , headingBDT
                      "Properties"
                      ""
                      [ valBDT "source" "Set the source colour" langSource
                      , valBDT "width" "Use this width for line" $ hoist1Drawing @A $ lineWidth
                      , valBDT "joinMitre" "Use a mitred line join, with limit" $ hoist1Drawing @A $ lineJoinMitre
                      , valBDT "joinRound" "Use a round line join" $ hoistDrawing @A $ lineJoinRound
                      , valBDT "joinBevel" "Use a bevel line join" $ hoistDrawing @A $ lineJoinBevel
                      , valBDT "capButt" "Use a butt line cap" $ hoistDrawing @A $ lineCapButt
                      , valBDT "capRound" "Use a round line cap" $ hoistDrawing @A $ lineCapRound
                      , valBDT "capSquare" "Use a square line cap" $ hoistDrawing @A $ lineCapSquare
                      , valBDT "dash" "Use a dash pattern for line" $ hoist2Drawing @A $ dash
                      , valBDT "fillRuleNonZero" "fill for non-zero winding number" $ hoistDrawing @A $ fillRuleWinding
                      , valBDT "fillRuleOdd" "fill for odd winding number" $ hoistDrawing @A $ fillRuleEvenOdd
                      , valBDT "fontFace" "" $ hoist3Drawing @A fontFace'
                      , valBDT "fontSize" "" $ hoist1Drawing @A fontSize
                      , namespaceBDT
                            "Operator"
                            ""
                            [ valBDT "clear" "" $ hoistDrawing @A operatorClear
                            , valBDT "source" "" $ hoistDrawing @A operatorSource
                            , valBDT "over" "" $ hoistDrawing @A operatorOver
                            , valBDT "in" "" $ hoistDrawing @A operatorIn
                            , valBDT "out" "" $ hoistDrawing @A operatorOut
                            , valBDT "atop" "" $ hoistDrawing @A operatorAtop
                            , valBDT "dest" "" $ hoistDrawing @A operatorDest
                            , valBDT "destOver" "" $ hoistDrawing @A operatorDestOver
                            , valBDT "destIn" "" $ hoistDrawing @A operatorDestIn
                            , valBDT "destOut" "" $ hoistDrawing @A operatorDestOut
                            , valBDT "destAtop" "" $ hoistDrawing @A operatorDestAtop
                            , valBDT "xor" "" $ hoistDrawing @A operatorXor
                            , valBDT "add" "" $ hoistDrawing @A operatorAdd
                            , valBDT "saturate" "" $ hoistDrawing @A operatorSaturate
                            , valBDT "multiply" "" $ hoistDrawing @A operatorMultiply
                            , valBDT "screen" "" $ hoistDrawing @A operatorScreen
                            , valBDT "overlay" "" $ hoistDrawing @A operatorOverlay
                            , valBDT "darken" "" $ hoistDrawing @A operatorDarken
                            , valBDT "lighten" "" $ hoistDrawing @A operatorLighten
                            , valBDT "colorDodge" "" $ hoistDrawing @A operatorColorDodge
                            , valBDT "colorBurn" "" $ hoistDrawing @A operatorColorBurn
                            , valBDT "hardLight" "" $ hoistDrawing @A operatorHardLight
                            , valBDT "softLight" "" $ hoistDrawing @A operatorSoftLight
                            , valBDT "difference" "" $ hoistDrawing @A operatorDifference
                            , valBDT "exclusion" "" $ hoistDrawing @A operatorExclusion
                            , valBDT "hslHue" "" $ hoistDrawing @A operatorHslHue
                            , valBDT "hslSaturation" "" $ hoistDrawing @A operatorHslSaturation
                            , valBDT "hslColor" "" $ hoistDrawing @A operatorHslColor
                            , valBDT "hslLuminosity" "" $ hoistDrawing @A operatorHslLuminosity
                            ]
                      ]
                , headingBDT
                      "Painting"
                      ""
                      [ valBDT "paint" "Paint everywhere (within the clip)" $ liftDrawing $ paint @[BottomType]
                      , valBDT "paintAlpha" "Paint everywhere with this alpha (within the clip)" $
                        lift1Drawing $ paintWithAlpha @[BottomType]
                      ]
                ]
              ]
        , headingBDT
              "Path"
              ""
              [ typeBDT "Path" "A path on a drawing." (MkSomeGroundType pathGroundType) []
              , namespaceBDT "Path" "" $
                monoidEntries @_ @LangPath <>
                [ headingBDT
                      "Construction"
                      ""
                      [ valBDT "close" "close the path into a loop" closePath
                      , valBDT "moveTo" "move to this point" moveTo
                      , valBDT "lineTo" "draw a line to this point" lineTo
                      , valBDT "curveTo" "draw a curve to this point" curveTo
                      , valBDT "relMoveTo" "move by this displacement" relMoveTo
                      , valBDT "relLineTo" "draw a line by this displacement" relLineTo
                      , valBDT "relCurveTo" "draw a curve by this displacement" relCurveTo
                      , valBDT "rectangle" "draw a rectangle" rectangle
                      , valBDT "arc" "`arc center radius angle1 angle2`" arc
                      , valBDT "arcNegative" "`arcNegative center radius angle1 angle2`" arcNegative
                      , valBDT "text" "" $ textPath
                      ]
                , headingBDT
                      "Drawing"
                      ""
                      [ valBDT "stroke" "Draw this path" $ lift1Drawing $ stroke @[BottomType]
                      , valBDT "fill" "Fill this path" $ lift1Drawing $ fill @[BottomType]
                      , valBDT "clip" "Clip drawing to this path" $ hoist1Drawing @A clip
                      ]
                ]
              ]
        , headingBDT
              "Patterns"
              ""
              [ typeBDT "Pattern" "" (MkSomeGroundType patternGroundType) []
              , namespaceBDT
                    "Pattern"
                    ""
                    [ valBDT "source" "" langPatternSource
                    , valBDT "mask" "" langPatternMask
                    , valBDT "solid" "" solidPattern
                    , valBDT "linear" "" $ \a b c -> liftPattern $ linearPattern a b c
                    , valBDT "radial" "" $ \a b c d e -> liftPattern $ radialPattern a b c d e
                    , headingBDT
                          "From Drawings"
                          ""
                          [ valBDT "colorDrawing" "" langColorDrawingPattern
                          , valBDT "alphaDrawing" "" langAlphaDrawingPattern
                          , valBDT "colorAlphaDrawing" "" langColorAlphaDrawingPattern
                          ]
                    ]
              ]
        ]

langPatternSource :: LangPattern -> LangDrawing A -> LangDrawing A
langPatternSource (MkLangPattern pat) (MkLangDrawing d) = MkLangDrawing $ patternSource pat d

langPatternMask :: LangPattern -> LangDrawing BottomType
langPatternMask (MkLangPattern pat) = MkLangDrawing $ patternMask pat

langColorDrawingPattern :: LangDrawing TopType -> LangPattern
langColorDrawingPattern (MkLangDrawing d) = MkLangPattern $ colorDrawingPattern d

langAlphaDrawingPattern :: LangDrawing TopType -> LangPattern
langAlphaDrawingPattern (MkLangDrawing d) = MkLangPattern $ alphaDrawingPattern d

langColorAlphaDrawingPattern :: LangDrawing TopType -> LangPattern
langColorAlphaDrawingPattern (MkLangDrawing d) = MkLangPattern $ colorAlphaDrawingPattern d
