{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Type.Equality
    (
    )
where

import Data.Shim
import Shapes

import Language.Expression.Dolan.Type.DolanType
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

data VarMatch
    = forall t. MkVarMatch
        (TypeVarT t)
        (TypeVarT t)

class MatchedEquality (f :: Type -> Type) where
    matchedTestEquality :: forall a b. [VarMatch] -> f a -> f b -> Maybe (a :~: b)

instance MatchedEquality TypeVarT where
    matchedTestEquality [] na nb = testEquality na nb
    matchedTestEquality (MkVarMatch va vb : vms) na nb =
        case (testEquality va na, testEquality vb nb) of
            (Just Refl, Just Refl) -> Just Refl
            (Nothing, Nothing) -> matchedTestEquality vms na nb
            _ -> Nothing

ccrArgumentMatchedTestEquality ::
    forall (ground :: GroundTypeKind) polarity (sv :: CCRVariance) (a :: CCRVarianceKind sv) (b :: CCRVarianceKind sv).
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    [VarMatch] ->
    CCRPolarArgument (DolanType ground) polarity sv a ->
    CCRPolarArgument (DolanType ground) polarity sv b ->
    Maybe (a :~: b)
ccrArgumentMatchedTestEquality vm (CoCCRPolarArgument arg1) (CoCCRPolarArgument arg2) =
    case polarityType @polarity of
        PositiveType -> matchedTestEquality vm arg1 arg2
        NegativeType -> matchedTestEquality vm arg1 arg2
ccrArgumentMatchedTestEquality vm (ContraCCRPolarArgument arg1) (ContraCCRPolarArgument arg2) =
    case polarityType @polarity of
        PositiveType -> matchedTestEquality vm arg1 arg2
        NegativeType -> matchedTestEquality vm arg1 arg2
ccrArgumentMatchedTestEquality vm (RangeCCRPolarArgument p1 q1) (RangeCCRPolarArgument p2 q2) =
    case polarityType @polarity of
        PositiveType -> do
            Refl <- matchedTestEquality vm p1 p2
            Refl <- matchedTestEquality vm q1 q2
            return Refl
        NegativeType -> do
            Refl <- matchedTestEquality vm p1 p2
            Refl <- matchedTestEquality vm q1 q2
            return Refl

instance
    forall (ground :: GroundTypeKind) polarity dv gt.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    MatchedEquality (CCRArguments (CCRPolarArgument (DolanType ground) polarity) dv gt)
    where
    matchedTestEquality _ NilCCRArguments NilCCRArguments = Just Refl
    matchedTestEquality vm (ConsCCRArguments ta tta) (ConsCCRArguments tb ttb) = do
        Refl <- ccrArgumentMatchedTestEquality vm ta tb
        Refl <- matchedTestEquality vm tta ttb
        return Refl

instance
    forall (ground :: GroundTypeKind) polarity.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    MatchedEquality (DolanType ground polarity)
    where
    matchedTestEquality _ NilDolanType NilDolanType = return Refl
    matchedTestEquality vm (ConsDolanType t1a tra) (ConsDolanType t1b trb) = do
        Refl <- matchedTestEquality vm t1a t1b
        Refl <- matchedTestEquality vm tra trb
        return Refl
    matchedTestEquality _ _ _ = Nothing

instance
    forall (ground :: GroundTypeKind) polarity.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    MatchedEquality (DolanGroundedType ground polarity)
    where
    matchedTestEquality vm (MkDolanGroundedType gta argsa) (MkDolanGroundedType gtb argsb) = do
        (Refl, HRefl) <- groundTypeTestEquality gta gtb
        Refl <- matchedTestEquality vm argsa argsb
        return Refl

instance
    forall (ground :: GroundTypeKind) polarity.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    MatchedEquality (DolanSingularType ground polarity)
    where
    matchedTestEquality vm (GroundedDolanSingularType ta) (GroundedDolanSingularType tb) = do
        Refl <- matchedTestEquality vm ta tb
        return Refl
    matchedTestEquality vm (VarDolanSingularType na) (VarDolanSingularType nb) = do
        Refl <- matchedTestEquality vm na nb
        return Refl
    matchedTestEquality vm (RecursiveDolanSingularType na pta) (RecursiveDolanSingularType nb ptb) =
        assignSameTypeVarT na nb $ do
            Refl <- matchedTestEquality (MkVarMatch na nb : vm) pta ptb
            return Refl
    matchedTestEquality _ _ _ = Nothing

instance
    forall (ground :: GroundTypeKind) polarity.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    TestEquality (DolanType ground polarity)
    where
    testEquality = matchedTestEquality []

instance
    forall (ground :: GroundTypeKind) polarity.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    TestEquality (DolanGroundedType ground polarity)
    where
    testEquality = matchedTestEquality []

instance
    forall (ground :: GroundTypeKind) polarity.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    TestEquality (DolanSingularType ground polarity)
    where
    testEquality = matchedTestEquality []
