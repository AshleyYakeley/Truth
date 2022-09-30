{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Media.Colour
    ( colourLibraryModule
    , LangColour
    , pattern ColorSRGB
    , pattern MkPerceptualSRGB16
    , pattern MkPerceptualSRGBFraction
    , LangAlphaColour
    , pattern Alpha
    , pattern MkAlphaColour16
    , pattern MkAlphaColourFraction
    , pattern MkOpaqueAlphaColour
    ) where

import Data.Shim
import Graphics.Color.Space
import Graphics.Color.Standard.SVG
import Pinafore.Base
import Pinafore.Language.API
import Shapes
import Shapes.Numeric

-- LangColour
type LangColour = Color (SRGB 'NonLinear) Word16

instance AsLiteral LangColour where
    literalCodec = alphaCodec . literalCodec

colourGroundType :: PinaforeGroundType '[] LangColour
colourGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangColour)|]) "Colour")
        {pgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance Is PolarityType polarity => HasPinaforeType polarity LangColour where
    pinaforeType = groundPinaforeType

instance HasPinaforeGroundType '[] LangColour where
    pinaforeGroundType = colourGroundType

-- LangColour
type LangAlphaColour = Color (Alpha (SRGB 'NonLinear)) Word16

alphaCodec :: Codec LangAlphaColour LangColour
alphaCodec =
    MkCodec
        (\case
             MkOpaqueAlphaColour col -> Just col
             _ -> Nothing)
        MkOpaqueAlphaColour

instance AsLiteral LangAlphaColour

instance AsMIMELiteral LangAlphaColour where
    literalMimeType = vndMIMEType "colour"
    literalContentSerializer = let
        fromT (r, (g, (b, a))) = Alpha (ColorSRGB r g b) a
        toT (Alpha (ColorSRGB r g b) a) = (r, (g, (b, a)))
        in invmap fromT toT $ serializer <***> serializer <***> serializer <***> serializer

alphaColourGroundType :: PinaforeGroundType '[] LangAlphaColour
alphaColourGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangAlphaColour)|]) "AlphaColour")
        {pgtGreatestDynamicSupertype = literalGreatestDynamicSupertype}

instance Is PolarityType polarity => HasPinaforeType polarity LangAlphaColour where
    pinaforeType = groundPinaforeType

instance HasPinaforeGroundType '[] LangAlphaColour where
    pinaforeGroundType = alphaColourGroundType

clamp16 :: Integer -> Word16
clamp16 x = fromInteger $ min (max 0 x) 65536

w16ToDouble :: Word16 -> Double
w16ToDouble n = (fromIntegral n) / 65535

doubleToW16 :: Double -> Word16
doubleToW16 n = clamp16 $ round $ n * 65535

w16ToDoubleColor :: Color (SRGB 'NonLinear) Word16 -> Color (SRGB 'NonLinear) Double
w16ToDoubleColor = fmap w16ToDouble

doubleToW16Color :: Color (SRGB 'NonLinear) Double -> Color (SRGB 'NonLinear) Word16
doubleToW16Color = fmap doubleToW16

pattern MkPerceptualSRGB16 ::
        Integer -> Integer -> Integer -> LangColour

pattern MkPerceptualSRGB16 r g b <-
        ColorSRGB (toInteger -> r) (toInteger -> g) (toInteger -> b)
  where MkPerceptualSRGB16 r g b
          = ColorSRGB (clamp16 r) (clamp16 g) (clamp16 b)

{-# COMPLETE MkPerceptualSRGB16 #-}

pattern MkPerceptualSRGBFraction ::
        Double -> Double -> Double -> LangColour

pattern MkPerceptualSRGBFraction r g b <-
        (w16ToDoubleColor -> ColorSRGB r g b)
  where MkPerceptualSRGBFraction r g b
          = doubleToW16Color $ ColorSRGB r g b

{-# COMPLETE MkPerceptualSRGBFraction #-}

pattern MkLinearRGBFraction ::
        Double -> Double -> Double -> LangColour

pattern MkLinearRGBFraction r g b <-
        (dcctf . w16ToDoubleColor -> ColorSRGB r g b)
  where MkLinearRGBFraction r g b
          = doubleToW16Color $ ecctf $ ColorSRGB r g b

{-# COMPLETE MkLinearRGBFraction #-}

pattern MkAlphaColour16 :: Integer -> LangColour -> LangAlphaColour

pattern MkAlphaColour16 op col <-
        ((\ acol -> (toInteger $ getAlpha acol, dropAlpha acol)) ->
           (op, col))
  where MkAlphaColour16 op col = addAlpha col $ clamp16 op

{-# COMPLETE MkAlphaColour16 #-}

pattern MkAlphaColourFraction ::
        Double -> LangColour -> LangAlphaColour

pattern MkAlphaColourFraction op col <-
        ((\ acol -> (w16ToDouble $ getAlpha acol, dropAlpha acol)) ->
           (op, col))
  where MkAlphaColourFraction op col = addAlpha col $ doubleToW16 op

{-# COMPLETE MkAlphaColourFraction #-}

pattern MkOpaqueAlphaColour :: LangColour -> LangAlphaColour

pattern MkOpaqueAlphaColour col = MkAlphaColour16 65535 col

fromSVGColor :: Color (SRGB 'NonLinear) Word8 -> LangColour
fromSVGColor = fmap (\n -> (fromIntegral n) * 257)

opaque :: LangColour -> LangAlphaColour
opaque = MkOpaqueAlphaColour

transparent :: LangAlphaColour
transparent = MkAlphaColour16 0 $ fromSVGColor black

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
        [ mkTypeEntry "Colour" "A human-perceivable colour." $ MkSomeGroundType colourGroundType
        , mkValPatEntry
              "SRGB16"
              "Construct a Colour from sRGB (perceptual) red, green, blue, integers in range 0 to 65535. (This is what it actually stored.)"
              MkPerceptualSRGB16 $
          PureFunction $ \(MkPerceptualSRGB16 r g b) -> (r, (g, (b, ())))
        , mkValPatEntry
              "SRGB"
              "Construct a Colour from sRGB (perceptual) red, green, blue, in range 0 to 1."
              MkPerceptualSRGBFraction $
          PureFunction $ \(MkPerceptualSRGBFraction r g b) -> (r, (g, (b, ())))
        , mkValPatEntry
              "LinearRGB"
              "Construct a Colour from linear red, green, blue, in range 0 to 1."
              MkLinearRGBFraction $
          PureFunction $ \(MkLinearRGBFraction r g b) -> (r, (g, (b, ())))
        , mkTypeEntry "AlphaColour" "A human-perceivable colour, with opacity." $ MkSomeGroundType alphaColourGroundType
        , literalSubtypeRelationEntry @LangAlphaColour
        , hasSubtypeRelationEntry @LangColour @LangAlphaColour Verify "A Colour is an opaque AlphaColour" $
          functionToShim "opaque" opaque
        , mkValPatEntry
              "MkAlphaColour16"
              "Construct an AlphaColour from an opacity in range 0 to 65535 and a Colour."
              MkAlphaColour16 $
          PureFunction $ \(MkAlphaColour16 op col) -> (op, (col, ()))
        , mkValPatEntry
              "MkAlphaColourFraction"
              "Construct an AlphaColour from an opacity in range 0 to 1 and a Colour."
              MkAlphaColourFraction $
          PureFunction $ \(MkAlphaColourFraction op col) -> (op, (col, ()))
        , mkValEntry "transparent" "The zero-opacity AlphaColour" transparent
        {- https://github.com/lehins/Color/issues/9
        , mkValEntry "over" "An AlphaColour over a Colour" $ over @Colour @Word16
        , mkValEntry "overA" "An AlphaColour over an AlphaColour" $ over @AlphaColour @Word16
        , mkValEntry "blend" "Blend two Colours by weight (of the first)" $ blend @Word16 @Colour
        , mkValEntry "blendA" "Blend two AlphaColours by weight (of the first)" $ blend @Word16 @AlphaColour
        , mkValEntry "darken" "Darken a Colour" $ darken @Colour @Word16
        , mkValEntry "darkenA" "Darken an AlphaColour" $ darken @AlphaColour @Word16
        -}
        , docTreeEntry
              "Named Colours"
              "SVG named colours, also used in CSS, from [SVG 1.1](https://www.w3.org/TR/SVG11/types.html#ColorKeywords)" $
          fmap mkNamedColourEntry allSVGColors
        ]
