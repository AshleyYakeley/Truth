module Pinafore.Language.Value.ListRef where

import Changes.Core
import Data.Shim
import Shapes

type LangListRef :: (Type, Type) -> Type
data LangListRef pq where
    OrderedLangListRef :: WModel (OrderedListUpdate [q] (ROWUpdate q)) -> LangListRef '( p, q)
    FullLangListRef
        :: WModel (BiUpdate (ListUpdate [p] (WholeUpdate p)) (ListUpdate [q] (WholeUpdate q))) -> LangListRef '( p, q)

instance CatFunctor (CatRange (->)) (->) LangListRef where
    cfmap (MkCatRange _ qq) (OrderedLangListRef model) =
        OrderedLangListRef $ eaMap (liftOrderedListChangeLens $ funcChangeLens qq) model
    cfmap (MkCatRange pp qq) (FullLangListRef model) =
        FullLangListRef $ eaMap (mappingBiChangeLens (listWholeChangeMap pp) (listWholeChangeMap qq)) model

instance HasVariance 'Rangevariance LangListRef where
    varianceRepresentational = Nothing

langListRefToOrdered :: LangListRef '( p, q) -> WModel (OrderedListUpdate [q] (ROWUpdate q))
langListRefToOrdered (OrderedLangListRef model) = model
langListRefToOrdered (FullLangListRef model) =
    eaMap
        (liftOrderedListChangeLens toReadOnlyChangeLens .
         listOrderedListChangeLens . fromReadOnlyRejectingChangeLens . biReadOnlyChangeLens)
        model
