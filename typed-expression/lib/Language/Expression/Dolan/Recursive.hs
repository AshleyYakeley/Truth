module Language.Expression.Dolan.Recursive where

import Data.Shim
import Shapes

type Recursive :: Type -> Type -> Type
newtype Recursive a t =
    MkRecursive ((t -> a) -> a)

instance Functor (Recursive a) where
    fmap pq (MkRecursive rp) = MkRecursive $ \callq -> rp $ \p -> callq $ pq p

instance RepresentationalRole (Recursive a) where
    representationalCoercion MkCoercion = MkCoercion

instance HasVariance 'Covariance (Recursive a) where
    varianceRepresentational = Just Dict

shimMapRecursive ::
       forall (pshim :: PolyShimKind) polarity a p q. (ApplyPolyShim pshim, Is PolarityType polarity)
    => PolarMap (pshim Type) polarity p q
    -> PolarMap (pshim Type) polarity (Recursive a p) (Recursive a q)
shimMapRecursive conv = polarMapTypeApply CovarianceType cid conv

recursiveIso ::
       forall (shim :: ShimKind Type) (t :: Type). FunctionShim shim
    => Isomorphism shim (Recursive t t) t
recursiveIso = let
    isoForwards = functionToShim "recursive-iso" $ \(MkRecursive taa) -> taa id
    isoBackwards = functionToShim "recursive-iso" $ \t -> MkRecursive $ \ta -> ta t
    in MkIsomorphism {..}

recursiveIsoNull ::
       forall (shim :: ShimKind Type) (a :: Type) (t :: Type). FunctionShim shim
    => Isomorphism shim (Recursive a t) t
recursiveIsoNull = let
    isoForwards = functionToShim "recursive-iso" $ \_ -> error "null"
    isoBackwards = functionToShim "recursive-iso" $ \t -> MkRecursive $ \ta -> ta t
    in MkIsomorphism {..}

newtype RecursiveF f =
    MkRecursiveF (forall a. (f a -> a) -> a)

unrollRecursiveF :: Functor f => RecursiveF f -> f (RecursiveF f)
unrollRecursiveF (MkRecursiveF faaa) = faaa $ fmap rollRecursiveF

rollRecursiveF :: Functor f => f (RecursiveF f) -> RecursiveF f
rollRecursiveF frf = MkRecursiveF $ \faa -> faa $ fmap (\(MkRecursiveF fbbb) -> fbbb faa) frf
