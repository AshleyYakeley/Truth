module Pinafore.Language.Library.Comparison
    ( comparisonLibSection
    ) where

import Import
import Pinafore.Language.Convert.HasType
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Var

comparisonLibSection :: LibraryStuff
comparisonLibSection =
    headingBDS
        "Comparison"
        ""
        [ headingBDS
              "Equivalence"
              "Equivalences should be reflexive, transitive, and symmetric. You can use Preorders and Orders as Equivalences (since they are subtypes)."
              [ typeBDS
                    "Equivalence"
                    ""
                    (qSomeGroundType @_ @Equivalence)
                    [valPatBDS "Mk" "" (MkEquivalence @A) $ PureFunction $ pure $ \(MkEquivalence @A o) -> (o, ())]
              , namespaceBDS "Equivalence" $
                monoidEntries @(Equivalence A) <>
                [ valBDS
                      "map"
                      "Map an equivalence by a function"
                      (contramap :: (B -> A) -> Equivalence A -> Equivalence B)
                , addNameInRootBDS $ valBDS "eqBy" "Equivalent." $ equivalent @A
                , addNameInRootBDS $ valBDS "neBy" "Not equivalent." $ notEquivalent @A
                ]
              ]
        , headingBDS
              "Preorder"
              "Preorders should be reflexive and transitive."
              [ typeBDS
                    "Preorder"
                    ""
                    (qSomeGroundType @_ @Preorder)
                    [valPatBDS "Mk" "" (MkPreorder @A) $ PureFunction $ pure $ \(MkPreorder @A o) -> (o, ())]
              , hasSubtypeRelationBDS @(Preorder A) @(Equivalence A) Verify "" $
                functionToShim "preorderEquivalence" $ preorderEquivalence @A
              , namespaceBDS
                    "Preorder"
                    [ valBDS "map" "Map a preorder by a function" (contramap :: (B -> A) -> Preorder A -> Preorder B)
                    , valBDS "reverse" "Reverse an order." $ reversePreorder @A
                    , valBDS "fromEquivalence" "" $ equivalencePreorder @A
                    , valBDS "compareBy" "" $ comparePreorder @A
                    , addNameInRootBDS $ valBDS "ltBy" "Strictly less." $ preorderLT @A
                    , addNameInRootBDS $ valBDS "leBy" "Less or equal." $ preorderLE @A
                    , addNameInRootBDS $ valBDS "gtBy" "Strictly greater." $ preorderGT @A
                    , addNameInRootBDS $ valBDS "geBy" "Greater or equal." $ preorderGE @A
                    ]
              ]
        , headingBDS
              "Order"
              "Strictly, total preorders rather than orders per se. They should be reflexive and transitive."
              [ typeBDS
                    "Order"
                    ""
                    (qSomeGroundType @_ @Order)
                    [valPatBDS "Mk" "" (MkOrder @A) $ PureFunction $ pure $ \(MkOrder @A o) -> (o, ())]
              , hasSubtypeRelationBDS @(Order A) @(Preorder A) Verify "" $
                functionToShim "orderPreorder" $ orderPreorder @A
              , namespaceBDS "Order" $
                monoidEntries @(Order A) <>
                [ valBDS "map" "Map an order by a function" (contramap :: (B -> A) -> Order A -> Order B)
                , valBDS "reverse" "Reverse an order." $ reverseOrder @A
                , addNameInRootBDS $
                  valBDS "indiscrete" "The total preorder that sees all things as equivalent." $ indiscreteOrder @A
                , addNameInRootBDS $ valBDS "compareBy" "" $ compareOrder @A
                , addNameInRootBDS $ valBDS "lesserBy" "Lesser of two." $ orderLesser @A
                , addNameInRootBDS $ valBDS "greaterBy" "Greater of two." $ orderGreater @A
                , addNameInRootBDS $ valBDS "leastBy" "Least of a list." $ orderLeast @A
                , addNameInRootBDS $ valBDS "greatestBy" "Greatest of a list." $ orderGreatest @A
                , addNameInRootBDS $ valBDS "sortBy" "Sort list" $ orderSort @A
                ]
              ]
        ]
