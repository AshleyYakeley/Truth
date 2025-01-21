module Language.Expression.Dolan.FreeVars where

import Data.Shim
import Shapes

import Language.Expression.TypeSystem

class FreeTypeVariables t where
    freeTypeVariables :: t -> ListSet SomeTypeVarT

instance FreeTypeVariables SomeTypeVarT where
    freeTypeVariables x = opoint x

instance FreeTypeVariables (TypeVarT t) where
    freeTypeVariables x = freeTypeVariables $ MkSomeTypeVarT x

instance FreeTypeVariables t => FreeTypeVariables [t] where
    freeTypeVariables x = concatmap freeTypeVariables x

instance FreeTypeVariables t => FreeTypeVariables (Maybe t) where
    freeTypeVariables (Just x) = freeTypeVariables x
    freeTypeVariables Nothing = mempty

instance FreeTypeVariables t => FreeTypeVariables (Result e t) where
    freeTypeVariables (SuccessResult x) = freeTypeVariables x
    freeTypeVariables (FailureResult _) = mempty

instance (forall t'. FreeTypeVariables (wit t')) => FreeTypeVariables (ShimWit shim wit t) where
    freeTypeVariables (MkShimWit wt _) = freeTypeVariables wt

instance
    (forall polarity' t'. FreeTypeVariables (ft polarity' t')) =>
    FreeTypeVariables (CCRPolarArgument ft polarity sv t)
    where
    freeTypeVariables (CoCCRPolarArgument t) = freeTypeVariables t
    freeTypeVariables (ContraCCRPolarArgument t) = freeTypeVariables t
    freeTypeVariables (RangeCCRPolarArgument p q) = freeTypeVariables p <> freeTypeVariables q

instance
    forall (w :: CCRArgumentKind) dv gt t.
    (forall sv a. FreeTypeVariables (w sv a)) =>
    FreeTypeVariables (CCRArguments w dv gt t)
    where
    freeTypeVariables NilCCRArguments = mempty
    freeTypeVariables (ConsCCRArguments arg1 argr) = freeTypeVariables arg1 <> freeTypeVariables argr

variableOccursIn ::
    forall t tv.
    FreeTypeVariables t =>
    TypeVarT tv ->
    t ->
    Bool
variableOccursIn var t = member (MkSomeTypeVarT var) $ freeTypeVariables t
