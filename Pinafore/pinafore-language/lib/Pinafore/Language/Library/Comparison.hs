module Pinafore.Language.Library.Comparison
    ( comparisonLibSection
    ) where

import Import
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Var

orderOn :: (B -> A) -> (A -> A -> Ordering) -> B -> B -> Ordering
orderOn ba order b1 b2 = order (ba b1) (ba b2)

comparisonLibSection :: LibraryStuff
comparisonLibSection =
    headingBDS
        "Comparison"
        ""
        [ headingBDS
              "Order"
              ""
              [ namespaceBDS "Order" $
                monoidEntries @(A -> A -> Ordering) <>
                [ valBDS "reverse" "Reverse an order." $ reverseOrder @A
                , addNameInRootBDS $ valBDS "lesser" "The lesser of two items in this order." $ lesser @A
                , addNameInRootBDS $ valBDS "greater" "The greater of two items in this order." $ greater @A
                , addNameInRootBDS $ valBDS "on" "Map an order by a function" orderOn
                ]
              ]
        ]
