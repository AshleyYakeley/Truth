module Pinafore.Language.Library.Entity.Text
    ( textEntityLibSection
    )
where

import Data.Text qualified
import Text.Collate qualified

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Action
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Blob ()
import Pinafore.Language.Library.Entity.Literal
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Optics ()
import Pinafore.Language.Library.Showable
import Pinafore.Language.Value

utf8Prism :: LangPrism' StrictByteString Text
utf8Prism = codecToPrism utf8Codec

collatorOrder :: Text.Collate.Collator -> Order Text
collatorOrder c = MkOrder $ Text.Collate.collate c

langOrder :: Text -> Result TextException (Order Text)
langOrder langcode =
    case Text.Collate.parseLang langcode of
        Right lang -> pure $ collatorOrder $ Text.Collate.collatorFor lang
        Left err -> FailureResult $ MkTextException $ pack err

rootOrder :: Order Text
rootOrder = collatorOrder Text.Collate.rootCollator

langQuote :: Text -> Text
langQuote = showT . unpack

textEntityLibSection :: LibraryStuff
textEntityLibSection =
    headingBDS
        "Text"
        ""
        [ typeBDS "Text" "" (MkSomeGroundType textGroundType) []
        , literalSubtypeRelationEntry @Text
        , showableSubtypeRelationEntry @Text "show as itself" id
        , namespaceBDS "Text"
            $ mconcat
                [ monoidEntries @Text
                , orderEntries
                    rootOrder
                    "Order alphabetical first, then lower case before upper, per Unicode normalisation."
                , sequenceEntries @Text
                ,
                    [ valBDS "toUpperCase" "" Data.Text.toUpper
                    , valBDS "toLowerCase" "" Data.Text.toLower
                    , valBDS "toTitleCase" "" Data.Text.toTitle
                    , valBDS "quote" "Quote with `\"` and escaping with `\\`" langQuote
                    , valBDS "langOrder" "Order for BCP 47 language tag" langOrder
                    , valBDS "caselessOrder" "Case-insensitive order" $ contramap Data.Text.toLower rootOrder
                    , valBDS "utf8" "Encode and decode UTF-8 from a `Blob`." utf8Prism
                    ]
                ]
        ]
