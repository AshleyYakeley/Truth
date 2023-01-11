{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.ModelOrder
    ( modelOrderLibSection
    ) where

import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Model
import Pinafore.Language.Library.Optics ()
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes

modelOrderLibSection :: BindDocTree context
modelOrderLibSection =
    headingBDT
        "ModelOrder"
        ""
        [ typeBDT "ModelOrder" "" (MkSomeGroundType modelOrderGroundType) []
        , hasSubtypeRelationBDT Verify "" $ functionToShim "Order to ModelOrder" $ pureLangModelOrder @A
        , namespaceBDT "ModelOrder" "" $
          monoidEntries @_ @(LangModelOrder A) <>
          [ valBDT
                "map"
                "Map a function on a `ModelOrder`."
                (contramap :: (B -> A) -> LangModelOrder A -> LangModelOrder B)
          , valBDT "on" "Order by a `ModelOrder` on a particular property." $ langModelOrderOn @B @A
          , valBDT "reverse" "Reverse a `ModelOrder`." $ reverseLangModelOrder @A
          , valBDT "whole" "Order two whole models." $ langModelOrderCompare @A
          ]
        ]
