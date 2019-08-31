module Pinafore.Language.Value.ListRef where

import Data.Shim
import Pinafore.Base
import Shapes
import Truth.Core

data PinaforeListRef (baseupdate :: Type) (pq :: (Type, Type)) where
    MkPinaforeListRef
        :: Range JMShim t pq
        -> PinaforeLensValue baseupdate (ListEdit [t] (WholeEdit t))
        -> PinaforeListRef baseupdate pq

instance CatFunctor (CatRange (->)) (->) (PinaforeListRef baseupdate) where
    cfmap f (MkPinaforeListRef r v) = MkPinaforeListRef (cfmap f r) v

instance HasVariance 'Rangevariance (PinaforeListRef baseupdate) where
    varianceRepresentational = Nothing
