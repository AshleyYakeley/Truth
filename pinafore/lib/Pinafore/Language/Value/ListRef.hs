module Pinafore.Language.Value.ListRef where

import Data.Shim
import Pinafore.Base
import Shapes
import Truth.Core

data PinaforeListRef (baseedit :: Type) (pq :: (Type, Type)) where
    MkPinaforeListRef
        :: Range JMShim t pq -> PinaforeLensValue baseedit (ListEdit [t] (WholeEdit t)) -> PinaforeListRef baseedit pq

instance CatFunctor (CatRange (->)) (->) (PinaforeListRef baseedit) where
    cfmap f (MkPinaforeListRef r v) = MkPinaforeListRef (cfmap f r) v

instance HasVariance 'Rangevariance (PinaforeListRef baseedit) where
    varianceRepresentational = Nothing
