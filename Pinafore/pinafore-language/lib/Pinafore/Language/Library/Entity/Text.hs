module Pinafore.Language.Library.Entity.Text
    ( textEntityLibSection
    ) where

import qualified Data.Text
import Import
import Pinafore.Language.Convert.Types
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Literal
import Pinafore.Language.Library.Entity.Showable
import Pinafore.Language.Library.LibraryModule
import qualified Text.Collate

textEntityLibSection :: LibraryStuff context
textEntityLibSection =
    headingBDS
        "Text"
        ""
        [ typeBDS "Text" "" (MkSomeGroundType textGroundType) []
        , literalSubtypeRelationEntry @Text
        , showableSubtypeRelationEntry @Text
        , namespaceBDS "Text" $
          monoidEntries @_ @Text <>
          orderEntries
              (Text.Collate.collate Text.Collate.rootCollator)
              "Order alphabetical first, then lower case before upper, per Unicode normalisation." <>
          [ valBDS "length" "The length of a text." $ olength @Text
          , valBDS "section" "`section start len text` is the section of `text` beginning at `start` of length `len`." $ \start len (text :: Text) ->
                take len $ drop start text
          , valBDS "toUpperCase" "" Data.Text.toUpper
          , valBDS "toLowerCase" "" Data.Text.toLower
          ]
        ]
