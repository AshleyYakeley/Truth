module Pinafore.Language.Library.List
    ( listLibSection
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

listLibSection :: LibraryStuff
listLibSection =
    headingBDS
        "List"
        ""
        [ typeBDS
            "List"
            "A list."
            (MkSomeGroundType listGroundType)
            [ addNameInRootBDS
                $ typeBDS "List1" "A list with at least one element." (MkSomeGroundType list1GroundType) []
            , hasSubtypeRelationBDS @(NonEmpty A) @[A] Verify "" $ functionToShim "NonEmpty.toList" toList
            , addNameInRootBDS
                $ valPatBDS "[]" "Empty list" ([] @BottomType)
                $ ImpureFunction
                $ pure
                $ \(v :: [A]) ->
                    case v of
                        [] -> Just ()
                        _ -> Nothing
            , addNameInRootBDS
                $ valPatBDS "::" "Construct a list" ((:|) @A)
                $ ImpureFunction
                $ pure
                $ \(v :: [A]) ->
                    case v of
                        a : b -> Just (a, (b, ()))
                        _ -> Nothing
            ]
        , hasSubtypeRelationBDS @[Entity] @Entity Verify "" $ functionToShim "listEntityConvert" listEntityConvert
        , hasSubtypeRelationBDS @[Showable] @Showable Verify "" $ functionToShim "show" textShowable
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
