module Pinafore.Language.Library.Entity.Ordering
    ( orderingEntityLibSection
    )
where

import Import
import Pinafore.Language.Convert.Types
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Literal
import Pinafore.Language.Library.Entity.Showable
import Pinafore.Language.Library.LibraryModule

orderingEntityLibSection :: LibraryStuff
orderingEntityLibSection =
    headingBDS
        "Ordering"
        ""
        [ typeBDS "Ordering" "" (MkSomeGroundType orderingGroundType)
            $ fmap
                addNameInRootBDS
                [ valPatBDS "LT" "Less than." LT
                    $ ImpureFunction
                    $ pure
                    $ \v ->
                        case v of
                            LT -> Just ()
                            _ -> Nothing
                , valPatBDS "EQ" "Equal to." EQ
                    $ ImpureFunction
                    $ pure
                    $ \v ->
                        case v of
                            EQ -> Just ()
                            _ -> Nothing
                , valPatBDS "GT" "Greater than." GT
                    $ ImpureFunction
                    $ pure
                    $ \v ->
                        case v of
                            GT -> Just ()
                            _ -> Nothing
                ]
        , literalSubtypeRelationEntry @Ordering
        , showableSubtypeRelationEntry @Ordering
        , namespaceBDS "Ordering"
            $ ordEntries @Ordering
            <> monoidEntries @Ordering
            <> [ addNameInRootBDS $ valBDS "eq" "Equal." $ (==) EQ
               , addNameInRootBDS $ valBDS "ne" "Not equal." $ (/=) EQ
               , addNameInRootBDS $ valBDS "lt" "Less than." $ (==) LT
               , addNameInRootBDS $ valBDS "le" "Less than or equal to." $ (/=) GT
               , addNameInRootBDS $ valBDS "gt" "Greater than." $ (==) GT
               , addNameInRootBDS $ valBDS "ge" "Greater than or equal to." $ (/=) LT
               ]
        ]
