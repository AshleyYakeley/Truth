module Pinafore.Language.Value.ListRef where

import Changes.Core
import Data.Shim
import Pinafore.Language.Shim
import Shapes

data LangListRef (pq :: (Type, Type)) where
    MkLangListRef :: Range (PinaforePolyShim Type) t pq -> WModel (ListEdit [t] (WholeEdit t)) -> LangListRef pq

instance CatFunctor (CatRange (->)) (->) LangListRef where
    cfmap f (MkLangListRef r v) = MkLangListRef (cfmap f r) v

instance HasVariance 'Rangevariance LangListRef where
    varianceRepresentational = Nothing
