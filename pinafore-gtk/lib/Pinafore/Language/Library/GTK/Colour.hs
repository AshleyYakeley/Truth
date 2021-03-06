{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.Colour
    ( colourLibraryModules
    , LangColour
    , pattern MkSRGBColour
    , LangAlphaColour
    , pattern MkLangAlphaColour
    ) where

import Data.Shim
import Graphics.Color.Space
import Graphics.Color.Standard.SVG
import Language.Expression.Dolan
import Pinafore.Language.API
import Shapes
import Shapes.Numeric

-- LangColour
type LangColour = Color (SRGB 'Linear) Double

colourGroundType :: PinaforeGroundType '[] LangColour
colourGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (HetEqual LangColour)|]) "Colour"

instance ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) LangColour where
    toShimWit = mkShimWit $ GroundDolanSingularType colourGroundType NilDolanArguments

instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) LangColour where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) LangColour where
    fromShimWit = mkShimWit $ GroundDolanSingularType colourGroundType NilDolanArguments

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) LangColour where
    fromShimWit = singleDolanShimWit fromJMShimWit

-- LangColour
type LangAlphaColour = Color (Alpha (SRGB 'Linear)) Double

alphaColourGroundType :: PinaforeGroundType '[] LangAlphaColour
alphaColourGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (HetEqual LangAlphaColour)|]) "AlphaColour"

instance ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) LangAlphaColour where
    toShimWit = mkShimWit $ GroundDolanSingularType alphaColourGroundType NilDolanArguments

instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) LangAlphaColour where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) LangAlphaColour where
    fromShimWit = mkShimWit $ GroundDolanSingularType alphaColourGroundType NilDolanArguments

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) LangAlphaColour where
    fromShimWit = singleDolanShimWit fromJMShimWit

pattern MkNLSRGB ::
        Double -> Double -> Double -> Color (SRGB 'NonLinear) Double

pattern MkNLSRGB r g b <- ColorSRGB r g b
  where MkNLSRGB r g b
          = ColorSRGB (clamp01 r) (clamp01 g) (clamp01 b)

{-# COMPLETE MkNLSRGB #-}

pattern MkSRGBColour :: Double -> Double -> Double -> LangColour

pattern MkSRGBColour r g b <- (ecctf -> MkNLSRGB r g b)
  where MkSRGBColour r g b = dcctf $ MkNLSRGB r g b

{-# COMPLETE MkSRGBColour #-}

pattern MkLinearRGBColour ::
        Double -> Double -> Double -> LangColour

pattern MkLinearRGBColour r g b <- (ColorSRGB r g b)
  where MkLinearRGBColour r g b = ColorSRGB r g b

{-# COMPLETE MkLinearRGBColour #-}

pattern MkLangAlphaColour ::
        Double -> LangColour -> LangAlphaColour

pattern MkLangAlphaColour op col <-
        ((\ acol -> (getAlpha acol, dropAlpha acol)) -> (op, col))
  where MkLangAlphaColour op col = addAlpha col op

{-# COMPLETE MkLangAlphaColour #-}

fromSVGColor :: Color (SRGB 'NonLinear) Word8 -> LangColour
fromSVGColor col = dcctf $ fmap (\n -> (fromIntegral n) / 255) col

opaque :: LangColour -> LangAlphaColour
opaque = MkLangAlphaColour 1

transparent :: LangAlphaColour
transparent = MkLangAlphaColour 0 $ fromSVGColor black

mkNamedColourEntry :: (String, Color (SRGB 'NonLinear) Word8) -> DocTreeEntry BindDoc
mkNamedColourEntry (name, colour) = let
    ColorSRGB r g b = colour
    tr = pack $ show r
    tg = pack $ show g
    tb = pack $ show b
    desc =
        rawMarkdown $
        "<span style=\"border: 1px solid black; background: rgb(" <>
        tr <>
        "," <>
        tg <>
        "," <> tb <> ")\">&nbsp;&nbsp;&nbsp;&nbsp;</span> SRGB " <> tr <> "\\/255 " <> tg <> "\\/255 " <> tb <> "\\/255"
    in mkValEntry (fromString name) desc $ fromSVGColor colour

colourLibraryModule :: LibraryModule
colourLibraryModule =
    MkDocTree
        "Colour"
        ""
        [ mkTypeEntry "Colour" "A human-perceivable colour." $ MkBoundType colourGroundType
        , mkValPatEntry
              "LinearRGB"
              "Construct a Colour from linear red, green, blue, in range 0 to 1."
              MkLinearRGBColour $ \(MkLinearRGBColour r g b) -> Just (r, (g, (b, ())))
        , mkValPatEntry
              "SRGB"
              "Construct a Colour from sRGB (perceptual) red, green, blue, in range 0 to 1."
              MkSRGBColour $ \(MkSRGBColour r g b) -> Just (r, (g, (b, ())))
        , mkTypeEntry "AlphaColour" "A human-perceivable colour, with opacity." $ MkBoundType alphaColourGroundType
        , mkSubtypeRelationEntry "Colour" "AlphaColour" "A Colour is an opaque AlphaColour" $
          pure $
          simpleSubtypeConversionEntry colourGroundType alphaColourGroundType $
          simpleSubtypeConversion $ functionToShim "opaque" opaque
        , mkValPatEntry "MkAlphaColour" "Construct an AlphaColour from an opacity and a Colour." MkLangAlphaColour $ \(MkLangAlphaColour op col) ->
              Just (op, (col, ()))
        , mkValEntry "transparent" "The zero-opacity AlphaColour" transparent
        {- https://github.com/lehins/Color/issues/9
        , mkValEntry "over" "An AlphaColour over a Colour" $ over @Colour @Double
        , mkValEntry "overA" "An AlphaColour over an AlphaColour" $ over @AlphaColour @Double
        , mkValEntry "blend" "Blend two Colours by weight (of the first)" $ blend @Double @Colour
        , mkValEntry "blendA" "Blend two AlphaColours by weight (of the first)" $ blend @Double @AlphaColour
        , mkValEntry "darken" "Darken a Colour" $ darken @Colour @Double
        , mkValEntry "darkenA" "Darken an AlphaColour" $ darken @AlphaColour @Double
        -}
        , docTreeEntry
              "Named Colours"
              "SVG named colours, also used in CSS, from [SVG 1.1](https://www.w3.org/TR/SVG11/types.html#ColorKeywords)" $
          fmap mkNamedColourEntry allSVGColors
        ]

colourLibraryModules :: [LibraryModule]
colourLibraryModules = [colourLibraryModule]
