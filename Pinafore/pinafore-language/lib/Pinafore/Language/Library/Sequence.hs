module Pinafore.Language.Library.Sequence
    ( sequenceLibSection
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule

append :: NonEmpty A -> [A] -> NonEmpty A
append (a :| aa) bb = a :| (aa <> bb)

mconcat1 :: NonEmpty (NonEmpty A) -> NonEmpty A
mconcat1 (na :| lna) = append na $ concatmap toList lna

linkShowable :: Link Showable Showable -> Showable
linkShowable NilLink = ListShowable []
linkShowable (ConsLink sa (ListShowable sb)) = ListShowable $ sa : sb
linkShowable (ConsLink sa (PlainShowable tb)) = PlainShowable $ showText sa <> "::" <> tb

sequenceLibSection :: LibraryStuff
sequenceLibSection =
    headingBDS
        "Sequence"
        ""
        [ typeBDS_ @_ @Link
            "Link"
            "A pair, or nil."
            []
        , hasSubtypeRelationBDS @(Link Entity Entity) @Entity Verify "" $ functionToShim "linkEntityConvert" linkEntityConvert
        , hasSubtypeRelationBDS @(Link Showable Showable) @Showable Verify "" $ functionToShim "show" linkShowable
        , hasSubtypeRelationBDS @() @(Link BottomType BottomType) Verify "" $ functionToShim "" $ \() -> NilLink
        , hasSubtypeRelationBDS @(A, B) @(Link A B) Verify "" $ functionToShim "" $ \(a, b) -> ConsLink a b
        , typeBDS_ @_ @LangList
            "List"
            "A list."
            [ hasBiNeutralSubtypeRelationBDS @(LangList A) @(Link A (Rec0 (Link A)))
            ]
        , typeBDS_ @_ @LangList1
            "List1"
            "A list with at least one element."
            [ hasBiNeutralSubtypeRelationBDS @(LangList1 A) @(A, Link A (Rec0 (Link A)))
            ]
        , namespaceBDS "List"
            $ mconcat
                [ pickNamesInRootBDS ["<>"] $ monoidEntries @[A]
                , fmap addNameInRootBDS $ elementsEntries @[A]
                , monadEntries @[]
                ,
                    [ valBDS "from" "Eliminate a list" $ \(fnil :: B) fcons (l :: [A]) ->
                        case l of
                            [] -> fnil
                            (a : aa) -> fcons a aa
                    , addNameInRootBDS
                        $ valBDS "maybeMap" "Map and filter a list." (mapMaybe :: (A -> Maybe B) -> [A] -> [B])
                    , addNameInRootBDS $ valBDS "zip" "Zip two lists." $ zip @A @B
                    ]
                ]
        , namespaceBDS "List1"
            $ applicativeEntries @NonEmpty
            <> [ valBDS "<>" "Concatenate a non-empty list with a list." append
               , addNameInRootBDS $ valBDS "concat1" "Concatenate a non-empty list of non-empty lists." mconcat1
               , valBDS
                    "sort"
                    "Sort non-empty list by an order."
                    (sortBy :: (A -> A -> Ordering) -> NonEmpty A -> NonEmpty A)
               ]
        ]
