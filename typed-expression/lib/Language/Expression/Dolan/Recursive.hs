module Language.Expression.Dolan.Recursive where

import Data.Shim
import Language.Expression.Common
import Shapes

type Recursive :: TF Type Type -> Type
newtype Recursive tf =
    MkRecursive (forall a. (Apply tf a -> a) -> a)

mapRecursive :: forall tf1 tf2. (forall a. Apply tf1 a -> Apply tf2 a) -> Recursive tf1 -> Recursive tf2
mapRecursive f (MkRecursive taa) = MkRecursive $ \(ta :: _ -> a) -> taa $ \t -> ta $ f @a t

rollRecursive :: forall (tf :: TF Type Type). ApplyFunctor tf -> Apply tf (Recursive tf) -> Recursive tf
rollRecursive afmap t = MkRecursive $ \ta -> ta $ unApplyFunctor afmap (\(MkRecursive taa :: Recursive tf) -> taa ta) t

unrollRecursive :: forall (tf :: TF Type Type). ApplyFunctor tf -> Recursive tf -> Apply tf (Recursive tf)
unrollRecursive afmap (MkRecursive taa) = taa $ unApplyFunctor afmap (rollRecursive @tf afmap)

rollRecursiveBijection ::
       forall (tf :: TF Type Type). ApplyFunctor tf -> Bijection (Recursive tf) (Apply tf (Recursive tf))
rollRecursiveBijection afmap = let
    isoForwards = unrollRecursive afmap
    isoBackwards = rollRecursive afmap
    in MkIsomorphism {..}

shimMapRecursive ::
       forall (shim :: ShimKind Type) polarity name p q. (IsoMapShim shim, Is PolarityType polarity)
    => SymbolType name
    -> PolarMap shim polarity p q
    -> PolarMap shim polarity (Recursive (USub name p)) (Recursive (USub name q))
shimMapRecursive var conv =
    withRefl (usubConstant @p var) $
    withRefl (usubConstant @q var) $ isoPolarMapShim "map-recursive" mapRecursive mapRecursive conv

rollRecursiveIsoShim ::
       forall (tf :: TF Type Type) (shim :: ShimKind Type). FunctionShim shim
    => ApplyFunctor tf
    -> Isomorphism shim (Recursive tf) (Apply tf (Recursive tf))
rollRecursiveIsoShim afmap = isoFunctionToShim "unroll" $ rollRecursiveBijection afmap

recursiveForceIso ::
       forall (shim :: ShimKind Type) (name :: Symbol) (t :: Type). (FunctionShim shim, UVarT name ~ t)
    => SymbolType name
    -> Isomorphism shim (Recursive (USub name t)) t
recursiveForceIso var =
    withRefl (usubIdentity @t var) $
    isoFunctionToShim "recursive" $ let
        isoForwards :: Recursive (USub name t) -> t
        isoForwards (MkRecursive taa) = taa id
        isoBackwards :: t -> Recursive (USub name t)
        isoBackwards t = MkRecursive $ \(ta :: _ a) -> assignUVar @Type @a var $ ta t
        in MkIsomorphism {..}

recursiveIsoNull ::
       forall (shim :: ShimKind Type) (name :: Symbol) (t :: Type). FunctionShim shim
    => SymbolType name
    -> Isomorphism shim (Recursive (USub name t)) t
recursiveIsoNull var =
    isoFunctionToShim "recursive-null" $ let
        isoForwards _ = error "null"
        isoBackwards t = withRefl (usubConstant @t var) $ MkRecursive $ \ta -> ta t
        in MkIsomorphism {..}
