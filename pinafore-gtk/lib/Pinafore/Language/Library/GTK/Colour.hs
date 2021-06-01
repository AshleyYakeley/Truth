{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GTK.Colour
    ( colourLibraryModule
    , LangColour
    , LangAlphaColour
    ) where

import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Colour.TH
import Shapes
import Shapes.Numeric

-- LangColour
type LangColour = Colour Double

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
type LangAlphaColour = AlphaColour Double

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

mkNamedColourEntry :: (String, LangColour) -> DocTreeEntry BindDoc
mkNamedColourEntry (name, colour) = let
    RGB r g b = toSRGB24 colour
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
    in mkValEntry (fromString name) desc colour

colourLibraryModule :: LibraryModule
colourLibraryModule =
    MkDocTree
        "Colour"
        ""
        [ mkTypeEntry "Colour" "A human-perceivable colour." $ MkBoundType colourGroundType
        , mkValPatEntry "SRGB" "Construct a Colour from sRGB red, green, blue, in range 0 to 1." (sRGB @Double) $ \colour -> let
              RGB r g b = toSRGB @Double colour
              in Just (r, (g, (b, ())))
        , mkTypeEntry "AlphaColour" "A human-perceivable colour, with opacity." $ MkBoundType alphaColourGroundType
        , mkSubtypeRelationEntry "Colour" "AlphaColour" "A Colour is an opaque AlphaColour" $
          pure $
          simpleSubtypeConversionEntry colourGroundType alphaColourGroundType $
          simpleSubtypeConversion $ functionToShim "opaque" opaque
        , mkValEntry "transparent" "The zero-opacity AlphaColour" $ transparent @Double
        , mkValEntry "withOpacity" "Construct an AlphaColour from a Colour with opacity" $ withOpacity @Double
        , mkValEntry "opacity" "The opacity of an AlphaColour" $ alphaChannel @Double
        , mkValEntry "over" "An AlphaColour over a Colour" $ over @Colour @Double
        , mkValEntry "overA" "An AlphaColour over an AlphaColour" $ over @AlphaColour @Double
        , mkValEntry "blend" "Blend two Colours by weight (of the first)" $ blend @Double @Colour
        , mkValEntry "blendA" "Blend two AlphaColours by weight (of the first)" $ blend @Double @AlphaColour
        , mkValEntry "darken" "Darken a Colour" $ darken @Colour @Double
        , mkValEntry "darkenA" "Darken an AlphaColour" $ darken @AlphaColour @Double
        , docTreeEntry "Named Colours" "" $ fmap mkNamedColourEntry $$(getColours)
        ]
