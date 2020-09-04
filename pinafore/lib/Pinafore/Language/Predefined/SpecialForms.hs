module Pinafore.Language.Predefined.SpecialForms
    ( special_forms
    ) where

import Pinafore.Language.DocTree
import Pinafore.Language.Predefined.Defs
import Shapes

mkSpecialFormEntry :: Text -> Text -> Text -> DocTreeEntry BindDoc
mkSpecialFormEntry docName docValueType docDescription = let
    docIsSupertype = False
    docIsPattern = False
    in EntryDocTreeEntry $ mkDefDocEntry MkDefDoc {..}

special_forms :: [DocTreeEntry BindDoc]
special_forms =
    [ docTreeEntry
          "Special Forms"
          "These are built-in keywords that resemble predefined bindings."
          [ mkSpecialFormEntry
                "property @A @B <anchor>"
                "A ~> B"
                "A property for this anchor. `A` and `B` are types that are subtypes of `Entity`."
          , mkSpecialFormEntry "entity @A <anchor>" "A" "An open entity for this anchor. `A` is an open entity type."
          , mkSpecialFormEntry
                "evaluate @A"
                "Text -> Action (Either Text A)"
                "A function that evaluates text as a Pinafore expression to be subsumed to positive type `A`.\n\
                \The result of the action is either the value (`Right`), or an error message (`Left`).\n\
                \The local scope is not in any way transmitted to the evaluation."
          ]
    ]
