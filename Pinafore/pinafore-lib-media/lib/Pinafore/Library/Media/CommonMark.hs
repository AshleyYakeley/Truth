module Pinafore.Library.Media.CommonMark
    ( CommonMarkText (..)
    , commonMarkStuff
    )
where

import Commonmark qualified as C
import Data.Shim
import Pinafore.API
import Shapes

import Pinafore.Library.Media.HTML
import Pinafore.Library.Media.Media

newtype CommonMarkText = MkCommonMarkText
    { unCommonMarkText :: Text
    }
    deriving newtype (Eq, Semigroup, Monoid, AsTypedLiteral)

instance AsLiteral CommonMarkText

-- CommonMarkText
commonMarkTextGroundType :: QGroundType '[] CommonMarkText
commonMarkTextGroundType =
    mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily CommonMarkText)|]) "CommonMarkText"

instance HasQGroundType '[] CommonMarkText where
    qGroundType = commonMarkTextGroundType

asText :: Codec Text CommonMarkText
asText = MkCodec (Just . MkCommonMarkText) unCommonMarkText

asMedia :: Codec Media CommonMarkText
asMedia =
    coerceCodec
        . mediaSpecificText
            (MkMediaType TextMediaType "markdown" [("variant", "CommonMark")])
            ( \case
                MkMediaType TextMediaType "markdown" _ -> True
                _ -> False
            )

toHTML :: CommonMarkText -> Result Text HTMLText
toHTML (MkCommonMarkText t) =
    mapResultFailure showText $ do
        html :: C.Html () <- eitherToResult $ C.commonmark "" t
        return $ MkHTMLText $ toStrict $ C.renderHtml html

commonMarkStuff :: LibraryStuff
commonMarkStuff =
    headingBDS "CommonMark" ""
        $ [ typeBDS
                "CommonMarkText"
                "Text that's intended to be CommonMark (not necessarily valid)."
                (MkSomeGroundType commonMarkTextGroundType)
                [valPatBDS "Mk" "" MkCommonMarkText $ PureFunction $ pure $ \(MkCommonMarkText t) -> (t, ())]
          , hasSubtypeRelationBDS @CommonMarkText @Text Verify "" $ functionToShim "unCommonMarkText" unCommonMarkText
          , namespaceBDS "CommonMarkText"
                $ monoidEntries @CommonMarkText
                <> [ valBDS "asText" "" $ codecToPrism asText
                   , valBDS "asMedia" "" $ codecToPrism asMedia
                   , valBDS "toHTML" "render as HTML" toHTML
                   ]
          ]
