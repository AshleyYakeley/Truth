{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Type.Equality
    (
    ) where

import Data.Shim
import Language.Expression.Dolan.Type.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             TestEquality (DolanType ground polarity) where
    testEquality NilDolanType NilDolanType = return Refl
    testEquality (ConsDolanType t1a tra) (ConsDolanType t1b trb) = do
        Refl <- testEquality t1a t1b
        Refl <- testEquality tra trb
        return Refl
    testEquality _ _ = Nothing

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             TestEquality (DolanGroundedType ground polarity) where
    testEquality (MkDolanGroundedType gta argsa) (MkDolanGroundedType gtb argsb) = do
        (Refl, HRefl) <- groundTypeTestEquality gta gtb
        Refl <- testEquality argsa argsb
        return Refl

instance forall (ground :: GroundTypeKind) polarity. (IsDolanGroundType ground, Is PolarityType polarity) =>
             TestEquality (DolanSingularType ground polarity) where
    testEquality (GroundedDolanSingularType ta) (GroundedDolanSingularType tb) = do
        Refl <- testEquality ta tb
        return Refl
    testEquality (VarDolanSingularType na) (VarDolanSingularType nb) = do
        Refl <- testEquality na nb
        return Refl
    testEquality (RecursiveDolanSingularType na pta) (RecursiveDolanSingularType nb ptb) = do
        Refl <- testEquality na nb
        Refl <- testEquality pta ptb
        return Refl
    testEquality _ _ = Nothing
