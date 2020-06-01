module Pinafore.Language.Value.ListRef where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Shim
import Shapes
import Truth.Core

data LangListRef (pq :: (Type, Type)) where
    MkLangListRef :: Range (PinaforeShim Type) t pq -> PinaforeRef (ListEdit [t] (WholeEdit t)) -> LangListRef pq

instance CatFunctor (CatRange (->)) (->) LangListRef where
    cfmap f (MkLangListRef r v) = MkLangListRef (cfmap f r) v

instance HasVariance 'Rangevariance LangListRef where
    varianceRepresentational = Nothing
