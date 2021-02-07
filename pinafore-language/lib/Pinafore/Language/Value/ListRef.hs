module Pinafore.Language.Value.ListRef where

import Changes.Core
import Data.Shim
import Pinafore.Language.Shim
import Shapes

type LangListRef :: (Type, Type) -> Type
data LangListRef pq where
    OrderedLangListRef :: WModel (OrderedListUpdate [q] (ROWUpdate q)) -> LangListRef '( p, q)
    FullLangListRef :: Range (PinaforePolyShim Type) t pq -> WModel (ListUpdate [t] (WholeUpdate t)) -> LangListRef pq

instance CatFunctor (CatRange (->)) (->) LangListRef where
    cfmap f (FullLangListRef r v) = FullLangListRef (cfmap f r) v
    cfmap (MkCatRange _ qq) (OrderedLangListRef v) =
        OrderedLangListRef $ eaMap (liftOrderedListChangeLens $ funcChangeLens qq) v

instance HasVariance 'Rangevariance LangListRef where
    varianceRepresentational = Nothing

langListRefToOrdered :: LangListRef '( p, q) -> WModel (OrderedListUpdate [q] (ROWUpdate q))
langListRefToOrdered (OrderedLangListRef model) = model
langListRefToOrdered (FullLangListRef (MkRange _ tq) model) =
    eaMap ((liftOrderedListChangeLens $ funcChangeLens $ shimToFunction tq) . listOrderedListChangeLens) model
