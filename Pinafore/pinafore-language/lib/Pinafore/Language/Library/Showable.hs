module Pinafore.Language.Library.Showable
    ( Showable (..)
    , showableSubtypeRelationEntry
    , showableLibSection
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type

newtype Showable = MkShowable {unShowable :: Text}

instance ShowText Showable where
    showText = unShowable

-- Showable
instance HasQGroundType '[] Showable where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Showable)|]) "Showable"

showableSubtypeRelationEntry ::
    forall a.
    HasQType QPolyShim 'Negative a =>
    RawMarkdown ->
    (a -> Text) ->
    LibraryStuff
showableSubtypeRelationEntry doc tt =
    hasSubtypeRelationBDS @a @Showable Verify doc
        $ functionToShim "toShowable"
        $ MkShowable
        . tt

showableLibSection :: LibraryStuff
showableLibSection =
    headingBDS
        "Showable"
        ""
        [ typeBDS
            "Showable"
            "Something that can be represented as `Text`."
            (qSomeGroundType @_ @Showable)
            [ valPatBDS "Mk" "" MkShowable $ PureFunction $ pure $ \(MkShowable t) -> (t, ())
            ]
        , namespaceBDS "Showable" [addNameInRootBDS $ valBDS "show" "Show something as `Text`" unShowable]
        ]
