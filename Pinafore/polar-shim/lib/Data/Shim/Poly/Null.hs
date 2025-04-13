module Data.Shim.Poly.Null where

import Shapes

import Data.Shim.Mono
import Data.Shim.Poly.Shim

type NullPolyShim :: PolyShimKind
data NullPolyShim k a b where
    MkNullPolyShim :: forall k (a :: k) (b :: k). NullPolyShim k a b

instance Show (NullPolyShim k a b) where
    show MkNullPolyShim = "null"

instance Category (NullPolyShim k) where
    id = MkNullPolyShim
    MkNullPolyShim . MkNullPolyShim = MkNullPolyShim

instance Groupoid (NullPolyShim k) where
    invert MkNullPolyShim = MkNullPolyShim

instance forall k (f :: Type -> k). CatFunctor (NullPolyShim Type) (NullPolyShim k) f where
    cfmap MkNullPolyShim = MkNullPolyShim

instance forall k (f :: Type -> k). CatFunctor (CatDual (NullPolyShim Type)) (NullPolyShim k) f where
    cfmap (MkCatDual MkNullPolyShim) = MkNullPolyShim

instance JoinMeetIsoShim (NullPolyShim Type)

instance JoinMeetShim (NullPolyShim Type) where
    initf = MkNullPolyShim
    termf = MkNullPolyShim
    join1 = MkNullPolyShim
    join2 = MkNullPolyShim
    joinf MkNullPolyShim MkNullPolyShim = MkNullPolyShim
    meet1 = MkNullPolyShim
    meet2 = MkNullPolyShim
    meetf MkNullPolyShim MkNullPolyShim = MkNullPolyShim

instance CoercibleKind k => IsoMapShim (NullPolyShim k) where
    isoMapShim _ _ _ MkNullPolyShim = MkNullPolyShim

instance CoercibleKind k => CoerceShim (NullPolyShim k) where
    coercionToShim _ _ = MkNullPolyShim
    shimToCoercion MkNullPolyShim = Nothing

instance CoercibleKind k => FunctionShim (NullPolyShim k) where
    functionToShim _ _ = MkNullPolyShim

instance LazyCategory (NullPolyShim Type) where
    iLazy _ = MkNullPolyShim

instance CartesianShim (NullPolyShim Type) where
    funcShim MkNullPolyShim MkNullPolyShim = MkNullPolyShim
    pairShim MkNullPolyShim MkNullPolyShim = MkNullPolyShim
    eitherShim MkNullPolyShim MkNullPolyShim = MkNullPolyShim
    shimExtractFunction MkNullPolyShim call = call MkNullPolyShim MkNullPolyShim

instance ReduciblePolyShim NullPolyShim where
    type ReducedPolyShim NullPolyShim = NullPolyShim
