{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.ModelOrder
    ( modelOrderLibSection
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Optics ()
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes

-- ModelOrder
modelOrderGroundType :: QGroundType '[ ContraCCRVariance] LangModelOrder
modelOrderGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangModelOrder)|]) "ModelOrder"

instance HasQGroundType '[ ContraCCRVariance] LangModelOrder where
    qGroundType = modelOrderGroundType

getFiniteSetModelList :: LangModelOrder A -> LangFiniteSetModel '( A, EnA) -> View (LangListModel '( TopType, A))
getFiniteSetModelList order val =
    modelOrderUpdateOrder order $ \(model :: Model update) uorder -> do
        let
            uo :: UpdateOrder (ContextUpdate update (ConstWholeUpdate EnA))
            uo =
                mapUpdateOrder
                    (liftContextChangeLens $ fromReadOnlyRejectingChangeLens . funcChangeLens (Known . meet2))
                    uorder
            rows :: Model (FiniteSetUpdate EnA)
            rows = unWModel $ unLangFiniteSetModel $ contraRangeLift meet2 val
            pkSub :: Model (ContextUpdate update (FiniteSetUpdate EnA))
            pkSub = contextModels model rows
        colSub :: Model (ContextUpdate update (OrderedListUpdate (ConstWholeUpdate EnA))) <-
            viewFloatMapModel (contextOrderedSetLens uo) pkSub
        return $
            OrderedLangListModel $
            eaMap (liftOrderedListChangeLens (constWholeChangeLens meet2) . tupleChangeLens SelectContent) $
            MkWModel colSub

modelOrderLibSection :: BindDocTree context
modelOrderLibSection =
    headingBDT
        "ModelOrder"
        ""
        [ typeBDT "ModelOrder" "" (MkSomeGroundType modelOrderGroundType) []
        , hasSubtypeRelationBDT Verify "" $ functionToShim "Order to ModelOrder" $ pureLangModelOrder @A
        , valBDT "orders" "Join `ModelOrder`s by priority." $ modelOrders @A
        , valBDT
              "mapOrder"
              "Map a function on a `ModelOrder`."
              (contramap :: (B -> A) -> LangModelOrder A -> LangModelOrder B)
        , valBDT "orderOn" "Order by a `ModelOrder` on a particular property." $ langModelOrderOn @B @A
        , valBDT "reverseOrder" "Reverse a `ModelOrder`." $ reverseLangModelOrder @A
        , valBDT "orderWhole" "Order two whole models." $ langModelOrderCompare @A
        , valBDT "finiteSetModelList" "All members of a finite set, by an order." $ finiteSetGetOrdered @A
        , valBDT
              "getFiniteSetModelList"
              "Get all members of a finite set, by an order. \
            \The resulting `ListModel` will behave more \"list-like\" than `finiteSetModelList`."
              getFiniteSetModelList
        ]
