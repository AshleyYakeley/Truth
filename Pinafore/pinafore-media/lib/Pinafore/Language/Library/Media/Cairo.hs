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

cairoStuff :: BindDocStuff ()
cairoStuff =
    headingBDS "Cairo" "" $
    pure $
    namespaceBDS
        "Cairo"
        [ headingBDS
              "Drawing"
              ""
              [ typeBDS "Drawing" "Something that can be drawn." (MkSomeGroundType drawingGroundType) []
              , namespaceBDS "Drawing" $
                monoidEntries @_ @(LangDrawing A) <>
                applicativeEntries @_ @LangDrawing <>
                [ valBDS "map" "" $ fmap @LangDrawing @A @B
                , valBDS "toImage" "" $ drawToImage @TopType
                , headingBDS
                      "Position"
                      ""
                      [ valBDS "position" "" langPointDrawing
                      , valBDS "ifPoint" "Restrict actions based on point" langIfPoint
                      , valBDS
                            "ifInRect"
                            "Restrict actions to within a rectangle, as ((left,top),(width,height))"
                            langIfInRect
                      ]
                , headingBDS
                      "Transformation"
                      ""
                      [ valBDS "translate" "Translate a drawing" $ hoist1Drawing @A translate
                      , valBDS "rotate" "Rotate a drawing" $ hoist1Drawing @A rotate
                      , valBDS "scale" "Scale a drawing" $ hoist1Drawing @A scale
                      ]
                , headingBDS
                      "Properties"
                      ""
                      [ valBDS "source" "Set the source colour" langSource
                      , valBDS "width" "Use this width for line" $ hoist1Drawing @A $ lineWidth
                      , valBDS "joinMitre" "Use a mitred line join, with limit" $ hoist1Drawing @A $ lineJoinMitre
                      , valBDS "joinRound" "Use a round line join" $ hoistDrawing @A $ lineJoinRound
                      , valBDS "joinBevel" "Use a bevel line join" $ hoistDrawing @A $ lineJoinBevel
                      , valBDS "capButt" "Use a butt line cap" $ hoistDrawing @A $ lineCapButt
                      , valBDS "capRound" "Use a round line cap" $ hoistDrawing @A $ lineCapRound
                      , valBDS "capSquare" "Use a square line cap" $ hoistDrawing @A $ lineCapSquare
                      , valBDS "dash" "Use a dash pattern for line" $ hoist2Drawing @A $ dash
                      , valBDS "fillRuleNonZero" "fill for non-zero winding number" $ hoistDrawing @A $ fillRuleWinding
                      , valBDS "fillRuleOdd" "fill for odd winding number" $ hoistDrawing @A $ fillRuleEvenOdd
                      , valBDS "fontFace" "" $ hoist3Drawing @A fontFace'
                      , valBDS "fontSize" "" $ hoist1Drawing @A fontSize
                      , namespaceBDS
                            "Operator"
                            [ valBDS "clear" "" $ hoistDrawing @A operatorClear
                            , valBDS "source" "" $ hoistDrawing @A operatorSource
                            , valBDS "over" "" $ hoistDrawing @A operatorOver
                            , valBDS "in" "" $ hoistDrawing @A operatorIn
                            , valBDS "out" "" $ hoistDrawing @A operatorOut
                            , valBDS "atop" "" $ hoistDrawing @A operatorAtop
                            , valBDS "dest" "" $ hoistDrawing @A operatorDest
                            , valBDS "destOver" "" $ hoistDrawing @A operatorDestOver
                            , valBDS "destIn" "" $ hoistDrawing @A operatorDestIn
                            , valBDS "destOut" "" $ hoistDrawing @A operatorDestOut
                            , valBDS "destAtop" "" $ hoistDrawing @A operatorDestAtop
                            , valBDS "xor" "" $ hoistDrawing @A operatorXor
                            , valBDS "add" "" $ hoistDrawing @A operatorAdd
                            , valBDS "saturate" "" $ hoistDrawing @A operatorSaturate
                            , valBDS "multiply" "" $ hoistDrawing @A operatorMultiply
                            , valBDS "screen" "" $ hoistDrawing @A operatorScreen
                            , valBDS "overlay" "" $ hoistDrawing @A operatorOverlay
                            , valBDS "darken" "" $ hoistDrawing @A operatorDarken
                            , valBDS "lighten" "" $ hoistDrawing @A operatorLighten
                            , valBDS "colorDodge" "" $ hoistDrawing @A operatorColorDodge
                            , valBDS "colorBurn" "" $ hoistDrawing @A operatorColorBurn
                            , valBDS "hardLight" "" $ hoistDrawing @A operatorHardLight
                            , valBDS "softLight" "" $ hoistDrawing @A operatorSoftLight
                            , valBDS "difference" "" $ hoistDrawing @A operatorDifference
                            , valBDS "exclusion" "" $ hoistDrawing @A operatorExclusion
                            , valBDS "hslHue" "" $ hoistDrawing @A operatorHslHue
                            , valBDS "hslSaturation" "" $ hoistDrawing @A operatorHslSaturation
                            , valBDS "hslColor" "" $ hoistDrawing @A operatorHslColor
                            , valBDS "hslLuminosity" "" $ hoistDrawing @A operatorHslLuminosity
                            ]
                      ]
                , headingBDS
                      "Painting"
                      ""
                      [ valBDS "paint" "Paint everywhere (within the clip)" $ liftDrawing $ paint @[BottomType]
                      , valBDS "paintAlpha" "Paint everywhere with this alpha (within the clip)" $
                        lift1Drawing $ paintWithAlpha @[BottomType]
                      ]
                ]
              ]
        , headingBDS
              "Path"
              ""
              [ typeBDS "Path" "A path on a drawing." (MkSomeGroundType pathGroundType) []
              , namespaceBDS "Path" $
                monoidEntries @_ @LangPath <>
                [ headingBDS
                      "Construction"
                      ""
                      [ valBDS "close" "close the path into a loop" closePath
                      , valBDS "moveTo" "move to this point" moveTo
                      , valBDS "lineTo" "draw a line to this point" lineTo
                      , valBDS "curveTo" "draw a curve to this point" curveTo
                      , valBDS "relMoveTo" "move by this displacement" relMoveTo
                      , valBDS "relLineTo" "draw a line by this displacement" relLineTo
                      , valBDS "relCurveTo" "draw a curve by this displacement" relCurveTo
                      , valBDS "rectangle" "draw a rectangle" rectangle
                      , valBDS "arc" "`arc center radius angle1 angle2`" arc
                      , valBDS "arcNegative" "`arcNegative center radius angle1 angle2`" arcNegative
                      , valBDS "text" "" $ textPath
                      ]
                , headingBDS
                      "Drawing"
                      ""
                      [ valBDS "stroke" "Draw this path" $ lift1Drawing $ stroke @[BottomType]
                      , valBDS "fill" "Fill this path" $ lift1Drawing $ fill @[BottomType]
                      , valBDS "clip" "Clip drawing to this path" $ hoist1Drawing @A clip
                      ]
                ]
              ]
        , headingBDS
              "Patterns"
              ""
              [ typeBDS "Pattern" "" (MkSomeGroundType patternGroundType) []
              , namespaceBDS
                    "Pattern"
                    [ valBDS "source" "" langPatternSource
                    , valBDS "mask" "" langPatternMask
                    , valBDS "solid" "" solidPattern
                    , valBDS "linear" "" $ \a b c -> liftPattern $ linearPattern a b c
                    , valBDS "radial" "" $ \a b c d e -> liftPattern $ radialPattern a b c d e
                    , headingBDS
                          "From Drawings"
                          ""
                          [ valBDS "colorDrawing" "" langColorDrawingPattern
                          , valBDS "alphaDrawing" "" langAlphaDrawingPattern
                          , valBDS "colorAlphaDrawing" "" langColorAlphaDrawingPattern
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
