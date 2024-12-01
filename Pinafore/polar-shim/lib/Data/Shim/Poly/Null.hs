module Data.Shim.Poly.Null where

import Data.Shim.Mono
import Data.Shim.Poly.Shim
import Shapes

type NullShim :: PolyShimKind
data NullShim k a b where
    MkNullShim :: forall k (a :: k) (b :: k). NullShim k a b

instance Show (NullShim k a b) where
    show MkNullShim = "null"

instance Category (NullShim k) where
    id = MkNullShim
    MkNullShim . MkNullShim = MkNullShim

instance Groupoid (NullShim k) where
    invert MkNullShim = MkNullShim

instance forall k (f :: Type -> k). CatFunctor (NullShim Type) (NullShim k) f where
    cfmap MkNullShim = MkNullShim

instance forall k (f :: Type -> k). CatFunctor (CatDual (NullShim Type)) (NullShim k) f where
    cfmap (MkCatDual MkNullShim) = MkNullShim

instance JoinMeetIsoShim (NullShim Type)

instance JoinMeetShim (NullShim Type) where
    initf = MkNullShim
    termf = MkNullShim
    join1 = MkNullShim
    join2 = MkNullShim
    joinf MkNullShim MkNullShim = MkNullShim
    meet1 = MkNullShim
    meet2 = MkNullShim
    meetf MkNullShim MkNullShim = MkNullShim

instance CoercibleKind k => IsoMapShim (NullShim k) where
    isoMapShim _ _ _ MkNullShim = MkNullShim

instance CoercibleKind k => CoerceShim (NullShim k) where
    coercionToShim _ _ = MkNullShim
    shimToCoercion MkNullShim = Nothing

instance CoercibleKind k => FunctionShim (NullShim k) where
    functionToShim _ _ = MkNullShim

instance LazyCategory (NullShim Type) where
    iLazy _ = MkNullShim

instance CartesianShim (NullShim Type) where
    funcShim MkNullShim MkNullShim = MkNullShim
    pairShim MkNullShim MkNullShim = MkNullShim
    eitherShim MkNullShim MkNullShim = MkNullShim
    shimExtractFunction MkNullShim call = call MkNullShim MkNullShim

instance ReduciblePolyShim NullShim where
    type ReducedPolyShim NullShim = NullShim
