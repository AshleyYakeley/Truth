module Pinafore.Language.Value.ListRef where

import Data.Shim
import Pinafore.Base
import Shapes
import Truth.Core

data PinaforeListRef (pq :: (Type, Type)) where
    MkPinaforeListRef :: Range JMShim t pq -> PinaforeValue (ListEdit [t] (WholeEdit t)) -> PinaforeListRef pq

instance CatFunctor (CatRange (->)) (->) PinaforeListRef where
    cfmap f (MkPinaforeListRef r v) = MkPinaforeListRef (cfmap f r) v

instance HasVariance 'Rangevariance PinaforeListRef where
    varianceRepresentational = Nothing
