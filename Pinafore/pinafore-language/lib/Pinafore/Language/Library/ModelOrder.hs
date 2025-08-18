{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.ModelOrder
    ( modelOrderLibSection
    )
where

import Import
import Pinafore.Language.Convert.Var
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Model
import Pinafore.Language.Library.Optics ()
import Pinafore.Language.Value

modelOrderLibSection :: LibraryStuff
modelOrderLibSection =
    headingBDS
        "ModelOrder"
        ""
        [ typeBDS "ModelOrder" "" (MkSomeGroundType modelOrderGroundType) []
        , hasSubtypeRelationBDS Verify "" $ functionToShim "Order to ModelOrder" $ pureLangModelOrder @A
        , namespaceBDS "ModelOrder"
            $ monoidEntries @(LangModelOrder A)
            <> [ valBDS
                    "map"
                    "Map a function on a `ModelOrder`."
                    (contramap :: (B -> A) -> LangModelOrder A -> LangModelOrder B)
               , valBDS "on" "Order by a `ModelOrder` on a particular property." $ langModelOrderOn @B @A
               , valBDS "reverse" "Reverse a `ModelOrder`." $ reverseLangModelOrder @A
               , valBDS "whole" "Order two whole models." $ langModelOrderCompare @A
               ]
        ]
