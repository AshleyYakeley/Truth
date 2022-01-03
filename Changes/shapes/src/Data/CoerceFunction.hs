module Data.CoerceFunction where

import Control.Category.Dual
import Data.CatFunctor
import Data.Coercion
import Data.KindMorphism
import Shapes.Import

data CoerceFunction (a :: k) (b :: k)
    = FuncCoerceFunction (KindFunction a b)
    | CoercionCoerceFunction (Coercion a b)

coerceFunctionToFunction ::
       forall k (a :: k) (b :: k). CoercibleKind k
    => CoerceFunction a b
    -> KindFunction a b
coerceFunctionToFunction (FuncCoerceFunction f) = f
coerceFunctionToFunction (CoercionCoerceFunction c) = coercionToFunction c

coerceFunction :: Coercible a b => CoerceFunction a b
coerceFunction = CoercionCoerceFunction MkCoercion

instance CoercibleKind k => Category (CoerceFunction :: k -> k -> Type) where
    id = CoercionCoerceFunction id
    (CoercionCoerceFunction ca) . (CoercionCoerceFunction cb) = CoercionCoerceFunction $ ca . cb
    cfa . cfb = FuncCoerceFunction $ coerceFunctionToFunction cfa . coerceFunctionToFunction cfb

instance (RepresentationalRole f, Functor f) => CatFunctor CoerceFunction CoerceFunction f where
    cfmap (FuncCoerceFunction ab) = FuncCoerceFunction $ cfmap ab
    cfmap (CoercionCoerceFunction ab) = CoercionCoerceFunction $ cfmap ab

instance (RepresentationalRole f, Contravariant f) => CatFunctor (CatDual CoerceFunction) CoerceFunction f where
    cfmap (MkCatDual (FuncCoerceFunction ab)) = FuncCoerceFunction $ ccontramap ab
    cfmap (MkCatDual (CoercionCoerceFunction ab)) = CoercionCoerceFunction $ ccontramap ab
