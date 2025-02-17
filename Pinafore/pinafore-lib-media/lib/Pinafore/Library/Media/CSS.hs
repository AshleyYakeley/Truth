module Pinafore.Library.Media.CSS
    ( CSSText (..)
    , cssStuff
    )
where

import Data.Shim
import Pinafore.API
import Shapes

import Pinafore.Library.Media.Media

newtype CSSText = MkCSSText
    { unCSSText :: Text
    }
    deriving newtype (Eq, Semigroup, Monoid, AsTypedLiteral)

instance AsLiteral CSSText

-- CSSText
instance HasQGroundType '[] CSSText where
    qGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily CSSText)|]) "CSSText"

asText :: Codec Text CSSText
asText = MkCodec (Just . MkCSSText) unCSSText

asMedia :: Codec Media CSSText
asMedia =
    coerceCodec
        . mediaSpecificText
            (MkMediaType TextMediaType "css" [])
            ( \case
                MkMediaType TextMediaType "css" _ -> True
                _ -> False
            )

rule :: Text -> [(Text, Text)] -> CSSText
rule sels decls = MkCSSText $ sels <> " {" <> concatmap (\(k, v) -> k <> ":" <> v <> ";") decls <> "}\n"

cssStuff :: LibraryStuff
cssStuff =
    headingBDS "CSS" ""
        $ [ typeBDS_
                @'[]
                @CSSText
                "CSSText"
                "Text that's intended to be CSS (not necessarily valid)."
                [valPatBDS "Mk" "" MkCSSText $ PureFunction $ pure $ \(MkCSSText t) -> (t, ())]
          , hasSubtypeRelationBDS @CSSText @Text Verify "" $ functionToShim "unCSSText" unCSSText
          , namespaceBDS "CSSText"
                $ monoidEntries @CSSText
                <> [ valBDS "rule" "A CSS rule." rule
                   , valBDS "asText" "" $ codecToPrism asText
                   , valBDS "asMedia" "" $ codecToPrism asMedia
                   ]
          ]
