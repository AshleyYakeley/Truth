module Pinafore.Language.Library.List
    ( listLibSection
    ) where

import Import
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type
import Pinafore.Language.Var

append :: NonEmpty A -> [A] -> NonEmpty A
append (a :| aa) bb = a :| (aa <> bb)

mconcat1 :: NonEmpty (NonEmpty A) -> NonEmpty A
mconcat1 (na :| lna) = append na $ concatmap toList lna

listLibSection :: LibraryStuff context
listLibSection =
    headingBDS
        "List"
        ""
        [ typeBDS
              "List"
              "A list."
              (MkSomeGroundType listGroundType)
              [ addNameInRootBDS $
                typeBDS "List1" "A list with at least one element." (MkSomeGroundType list1GroundType) []
              , hasSubtypeRelationBDS @(NonEmpty A) @[A] Verify "" $ functionToShim "NonEmpty.toList" toList
              , addNameInRootBDS $
                valPatBDS "[]" "Empty list" ([] @BottomType) $
                ImpureFunction $ \(v :: [A]) ->
                    case v of
                        [] -> Just ()
                        _ -> Nothing
              , addNameInRootBDS $
                valPatBDS "::" "Construct a list" ((:|) @A) $
                ImpureFunction $ \(v :: [A]) ->
                    case v of
                        a:b -> Just (a, (b, ()))
                        _ -> Nothing
              ]
        , hasSubtypeRelationBDS @[Entity] @Entity Verify "" $ functionToShim "listEntityConvert" listEntityConvert
        , hasSubtypeRelationBDS @[Showable] @Showable Verify "" $ functionToShim "show" textShowable
        , namespaceBDS "List" $
          pickNamesInRootBDS ["<>"] (monoidEntries @_ @[A]) <>
          monadEntries @_ @[] <>
          [ valBDS "from" "Eliminate a list" $ \(fnil :: B) fcons (l :: [A]) ->
                case l of
                    [] -> fnil
                    (a:aa) -> fcons a aa
          , addNameInRootBDS $ valBDS "length" "Number of items in a list" (length :: [TopType] -> Int)
          , addNameInRootBDS $ valBDS "index" "Get item from list by index." (index :: [A] -> Int -> Maybe A)
          , addNameInRootBDS $ valBDS "filter" "Filter a list." (filter :: (A -> Bool) -> [A] -> [A])
          , addNameInRootBDS $ valBDS "maybeMap" "Map and filter a list." (mapMaybe :: (A -> Maybe B) -> [A] -> [B])
          , addNameInRootBDS $
            valBDS "section" "`section start len x` is the section of `x` beginning at `start` of length `len`." $ \start len (x :: [A]) ->
                take len $ drop start x
          , addNameInRootBDS $ valBDS "take" "Take the first n elements." (take :: Int -> [A] -> [A])
          , addNameInRootBDS $ valBDS "drop" "Drop the first n elements." (drop :: Int -> [A] -> [A])
          , addNameInRootBDS $
            valBDS "takeWhile" "Take while the condition holds." (takeWhile :: (A -> Bool) -> [A] -> [A])
          , addNameInRootBDS $
            valBDS "dropWhile" "Drop while the condition holds." (dropWhile :: (A -> Bool) -> [A] -> [A])
          , addNameInRootBDS $ valBDS "zip" "Zip two lists." $ zip @A @B
          , addNameInRootBDS $ valBDS "sort" "Sort list by an order." (sortBy :: (A -> A -> Ordering) -> [A] -> [A])
          ]
        , namespaceBDS "List1" $
          applicativeEntries @_ @NonEmpty <>
          [ valBDS "<>" "Concatenate a non-empty list with a list." append
          , addNameInRootBDS $ valBDS "concat1" "Concatenate a non-empty list of non-empty lists." mconcat1
          , valBDS
                "sort"
                "Sort non-empty list by an order."
                (sortBy :: (A -> A -> Ordering) -> NonEmpty A -> NonEmpty A)
          ]
        ]
