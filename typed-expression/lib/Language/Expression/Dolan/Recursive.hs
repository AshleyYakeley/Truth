module Language.Expression.Dolan.Recursive where

import Data.Shim
import Language.Expression.Common
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
recursiveIso =
    isoFunctionToShim "recursive" $ let
        isoForwards (MkRecursive taa) = taa id
        isoBackwards t = MkRecursive $ \ta -> ta t
        in MkIsomorphism {..}

recursiveIsoNull ::
       forall (shim :: ShimKind Type) (a :: Type) (t :: Type). FunctionShim shim
    => Isomorphism shim (Recursive a t) t
recursiveIsoNull =
    isoFunctionToShim "recursive-null" $ let
        isoForwards _ = error "null"
        isoBackwards t = MkRecursive $ \ta -> ta t
        in MkIsomorphism {..}

unrollRecursiveBijection ::
       SymbolType name
    -> (Bijection (UVar Type name) (Recursive (UVar Type name) t) -> Bijection t p)
    -> Bijection (Recursive (UVar Type name) t) p
unrollRecursiveBijection _var _convf = let
    isoForwards _ = error "NYI: unroll"
    isoBackwards _ = error "NYI: unroll"
    in MkIsomorphism {..}

unrollRecursiveIsoShim ::
       FunctionShim shim
    => SymbolType name
    -> (Isomorphism shim (UVar Type name) (Recursive (UVar Type name) t) -> Isomorphism shim t p)
    -> Isomorphism shim (Recursive (UVar Type name) t) p
unrollRecursiveIsoShim var convf =
    isoFunctionToShim "unroll" $ unrollRecursiveBijection var $ isoShimToFunction . convf . isoFunctionToShim "unroll"

newtype RecursiveF f =
    MkRecursiveF (forall a. (f a -> a) -> a)

unrollRecursiveF :: Functor f => RecursiveF f -> f (RecursiveF f)
unrollRecursiveF (MkRecursiveF faaa) = faaa $ fmap rollRecursiveF

rollRecursiveF :: Functor f => f (RecursiveF f) -> RecursiveF f
rollRecursiveF frf = MkRecursiveF $ \faa -> faa $ fmap (\(MkRecursiveF fbbb) -> fbbb faa) frf
