module Pinafore.Library.Media.HTML
    ( HTMLText(..)
    , htmlStuff
    ) where

import Changes.World.Media.Type
import Data.Shim
import Pinafore.API
import Pinafore.Library.Media.Media
import Shapes

newtype HTMLText = MkHTMLText
    { unHTMLText :: Text
    } deriving newtype (Eq, Semigroup, Monoid, AsTypedLiteral)

instance AsLiteral HTMLText

-- HTMLText
htmlTextGroundType :: QGroundType '[] HTMLText
htmlTextGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily HTMLText)|]) "HTMLText"

instance HasQGroundType '[] HTMLText where
    qGroundType = htmlTextGroundType

asText :: Codec Text HTMLText
asText = MkCodec (Just . MkHTMLText) unHTMLText

asMedia :: Codec Media HTMLText
asMedia =
    coerceCodec .
    mediaSpecificText
        (MkMediaType ApplicationMediaType "html" [])
        (\case
             MkMediaType TextMediaType "html" _ -> True
             MkMediaType ApplicationMediaType "html" _ -> True
             _ -> False)

escapeChar :: Char -> String
escapeChar '&' = "&amp;"
escapeChar '<' = "&lt;"
escapeChar '>' = "&gt;"
escapeChar '\'' = "&apos;"
escapeChar '"' = "&quot;"
escapeChar c = [c]

escapeText :: Text -> Text
escapeText = pack . concatmap escapeChar . unpack

plainEscapeChar :: Char -> String
plainEscapeChar '&' = "&amp;"
plainEscapeChar '<' = "&lt;"
plainEscapeChar '>' = "&gt;"
plainEscapeChar '\'' = "&apos;"
plainEscapeChar '"' = "&quot;"
plainEscapeChar '\n' = "<br />\n"
plainEscapeChar c = [c]

plainEscapeText :: Text -> Text
plainEscapeText = pack . concatmap plainEscapeChar . unpack

plain :: Text -> HTMLText
plain t = MkHTMLText $ plainEscapeText t

attrText :: [(Text, Text)] -> Text
attrText aa = concatmap (\(k, v) -> " " <> k <> "=\"" <> escapeText v <> "\"") aa

tagAttrs :: Text -> [(Text, Text)] -> HTMLText -> HTMLText
tagAttrs e aa (MkHTMLText "") = MkHTMLText $ "<" <> e <> attrText aa <> " />"
tagAttrs e aa (MkHTMLText t) = MkHTMLText $ "<" <> e <> attrText aa <> ">" <> t <> "</" <> e <> ">"

tag :: Text -> HTMLText -> HTMLText
tag e = tagAttrs e []

htmlStuff :: LibraryStuff ()
htmlStuff =
    headingBDS "HTML" "" $
    [ typeBDS
          "HTMLText"
          "Text that's intended to be HTML (not necessarily valid)."
          (MkSomeGroundType htmlTextGroundType)
          [valPatBDS "Mk" "" MkHTMLText $ PureFunction $ pure $ \(MkHTMLText t) -> (t, ())]
    , hasSubtypeRelationBDS @HTMLText @Text Verify "" $ functionToShim "unHTMLText" unHTMLText
    , namespaceBDS "HTMLText" $
      monoidEntries @_ @HTMLText <>
      [ valBDS "plain" "Plain text HTML." plain
      , valBDS "tag" "Tag HTML to create an element." tag
      , valBDS "tagAttrs" "Tag HTML with attributes to create an element." tagAttrs
      , valBDS "asText" "" $ codecToPrism asText
      , valBDS "asMedia" "" $ codecToPrism asMedia
      ]
    ]
