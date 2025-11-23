module Pinafore.Language.Library.Entity.Showable
    ( showableSubtypeRelationEntry
    , showableEntityLibSection
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type

showableSubtypeRelationEntry ::
    forall a.
    (HasQType QPolyShim 'Negative a, ShowText a) =>
    LibraryStuff
showableSubtypeRelationEntry = hasSubtypeRelationBDS @a @Showable Verify "" $ functionToShim "textShowable" textShowable

showableEntityLibSection :: LibraryStuff
showableEntityLibSection =
    headingBDS
        "Showable"
        ""
        [ typeBDS
            "Showable"
            "Something that can be represented as `Text`."
            (MkSomeGroundType showableGroundType)
            [ valPatBDS "Text" "" PlainShowable $ ImpureFunction $ pure $ \case
                PlainShowable t -> Just (t, ())
                _ -> Nothing
            , valPatBDS "List" "" ListShowable $ ImpureFunction $ pure $ \case
                ListShowable t -> Just (t, ())
                _ -> Nothing
            ]
        , namespaceBDS "Showable" [addNameInRootBDS $ valBDS "show" "Show something as `Text`" $ showText @Showable]
        ]
