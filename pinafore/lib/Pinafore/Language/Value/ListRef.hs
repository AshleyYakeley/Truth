module Pinafore.Language.Value.ListRef where

import Data.Shim
import Pinafore.Base
import Shapes
import Truth.Core

data LangListRef (pq :: (Type, Type)) where
    MkLangListRef :: Range JMShim t pq -> PinaforeRef (ListEdit [t] (WholeEdit t)) -> LangListRef pq

instance CatFunctor (CatRange (->)) (->) LangListRef where
    cfmap f (MkLangListRef r v) = MkLangListRef (cfmap f r) v

instance HasVariance 'Rangevariance LangListRef where
    varianceRepresentational = Nothing
