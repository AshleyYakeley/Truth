module Pinafore.Language.Library.Entity.Text
    ( textEntityLibSection
    ) where

import Data.Text qualified
import Import
import Pinafore.Language.Convert.Types
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Blob ()
import Pinafore.Language.Library.Entity.Literal
import Pinafore.Language.Library.Entity.Showable
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Optics ()
import Pinafore.Language.Value
import Text.Collate qualified

utf8Prism :: LangPrism' StrictByteString Text
utf8Prism = codecToPrism utf8Codec

textEntityLibSection :: LibraryStuff
textEntityLibSection =
    headingBDS
        "Text"
        ""
        [ typeBDS "Text" "" (MkSomeGroundType textGroundType) []
        , literalSubtypeRelationEntry @Text
        , showableSubtypeRelationEntry @Text
        , namespaceBDS "Text" $
          mconcat
              [ monoidEntries @Text
              , orderEntries
                    (Text.Collate.collate Text.Collate.rootCollator)
                    "Order alphabetical first, then lower case before upper, per Unicode normalisation."
              , sequenceEntries @Text
              , [ valBDS "toUpperCase" "" Data.Text.toUpper
                , valBDS "toLowerCase" "" Data.Text.toLower
                , valBDS "utf8" "Encode and decode UTF-8 from a `Blob`." utf8Prism
                ]
              ]
        ]
