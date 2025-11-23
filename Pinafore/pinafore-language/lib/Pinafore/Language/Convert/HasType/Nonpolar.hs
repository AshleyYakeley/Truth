{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-overlapping-patterns #-}

module Pinafore.Language.Convert.HasType.Nonpolar where

import Language.Expression.Dolan

import Import
import Pinafore.Language.Convert.HasType.Ground
import Pinafore.Language.Type

type HasQNonpolarType :: Type -> Constraint
class HasQNonpolarType t where
    qNonpolarType :: ShimWit Coercion QNonpolarType t

instance
    {-# OVERLAPPABLE #-}
    forall t.
    HasQNonpolarPartialGroundedType t =>
    HasQNonpolarType t
    where
    qNonpolarType = reShimWit GroundedNonpolarType qNonpolarGroundedType

type HasQNonpolarTypeArgument :: forall (sv :: CCRVariance) -> CCRVarianceKind sv -> Constraint
class HasQNonpolarTypeArgument sv t | t -> sv where
    qNonpolarTypeArgument :: ShimWit Coercion (QNonpolarTypeArgument sv) t

instance
    forall (t :: Type).
    HasQNonpolarType t =>
    HasQNonpolarTypeArgument CoCCRVariance t
    where
    qNonpolarTypeArgument = case qNonpolarType @t of
        MkShimWit t MkCoercion -> MkShimWit (CoNonpolarArgument t) MkCoercion

instance
    forall (t :: Type).
    HasQNonpolarType t =>
    HasQNonpolarTypeArgument ContraCCRVariance t
    where
    qNonpolarTypeArgument = case qNonpolarType @t of
        MkShimWit t MkCoercion -> MkShimWit (ContraNonpolarArgument t) MkCoercion

instance
    forall (p :: Type) (q :: Type).
    (HasQNonpolarType p, HasQNonpolarType q) =>
    HasQNonpolarTypeArgument 'RangeCCRVariance '(p, q)
    where
    qNonpolarTypeArgument = case qNonpolarType @p of
        MkShimWit tp MkCoercion -> case qNonpolarType @q of
            MkShimWit tq MkCoercion -> MkShimWit (RangeNonpolarArgument tp tq) MkCoercion

type HetQNonpolarPartialGroundedType :: forall k. CCRVariances -> k -> Type
data HetQNonpolarPartialGroundedType dv t where
    MkHetQNonpolarPartialGroundedType ::
        forall dv (t :: CCRVariancesKind dv).
        QNonpolarPartialGroundedType dv t ->
        HetQNonpolarPartialGroundedType dv t

runHetQNonpolarPartialGroundedType ::
    forall dv (t :: CCRVariancesKind dv).
    HetQNonpolarPartialGroundedType dv t ->
    QNonpolarPartialGroundedType dv t
runHetQNonpolarPartialGroundedType (MkHetQNonpolarPartialGroundedType pgt) = pgt

type HasQNonpolarPartialGroundedType :: forall k. k -> Constraint
class HasQNonpolarPartialGroundedType (t :: k) where
    hetQNonpolarPartialGroundedType :: ShimWit Coercion (HetQNonpolarPartialGroundedType (HetCCRVariancesOf t)) t

qNonpolarPartialGroundedType ::
    forall dv (t :: CCRVariancesKind dv).
    (HasQNonpolarPartialGroundedType t, dv ~ HetCCRVariancesOf t) =>
    ShimWit Coercion (QNonpolarPartialGroundedType dv) t
qNonpolarPartialGroundedType = reShimWit runHetQNonpolarPartialGroundedType hetQNonpolarPartialGroundedType

qNonpolarGroundedType :: forall (t :: Type). HasQNonpolarPartialGroundedType t => ShimWit Coercion QNonpolarGroundedType t
qNonpolarGroundedType = reShimWit nonpolarPartialToGroundedType qNonpolarPartialGroundedType

instance {-# OVERLAPPABLE #-} forall k (f :: k). HetConstraint (HasQGroundType (HetCCRVariancesOf f)) f => HasQNonpolarPartialGroundedType f where
    hetQNonpolarPartialGroundedType =
        case hetConstraint @_ @(HasQGroundType (HetCCRVariancesOf f)) @_ @f of
            MkHetConstraintWitness -> mkShimWit $ MkHetQNonpolarPartialGroundedType $ GroundNonpolarPartialGroundedType qGroundType

instance
    forall dv k (f :: Type -> k) (a :: Type).
    ( RepresentationalRole f
    , k ~ CCRVariancesKind dv
    , HetCCRVariancesOf f ~ ('SimpleCCRVariance (VarianceOf f) ': dv)
    , HasQNonpolarPartialGroundedType f
    , HetCCRVariancesOf (f a) ~ dv
    , HasQNonpolarTypeArgument ('SimpleCCRVariance (VarianceOf f)) a
    ) =>
    HasQNonpolarPartialGroundedType (f a)
    where
    hetQNonpolarPartialGroundedType = case hetQNonpolarPartialGroundedType @(Type -> k) @f of
        MkShimWit (MkHetQNonpolarPartialGroundedType tf) convf ->
            case qNonpolarTypeArgument @('SimpleCCRVariance (VarianceOf f)) @a of
                MkShimWit targ conva ->
                    MkShimWit (MkHetQNonpolarPartialGroundedType $ ApplyNonpolarPartialGroundedType tf targ)
                        $ applyCoercion1 convf conva

instance
    forall dv k (f :: (Type, Type) -> k) (a :: (Type, Type)).
    ( HasCCRVariance 'RangeCCRVariance f
    , RepresentationalRole f
    , k ~ CCRVariancesKind dv
    , HetCCRVariancesOf f ~ ('RangeCCRVariance ': dv)
    , HasQNonpolarPartialGroundedType f
    , HetCCRVariancesOf (f a) ~ dv
    , HasQNonpolarTypeArgument 'RangeCCRVariance a
    ) =>
    HasQNonpolarPartialGroundedType (f a)
    where
    hetQNonpolarPartialGroundedType = case hetQNonpolarPartialGroundedType @((Type, Type) -> k) @f of
        MkShimWit (MkHetQNonpolarPartialGroundedType tf) convf ->
            case qNonpolarTypeArgument @'RangeCCRVariance @a of
                MkShimWit targ conva ->
                    MkShimWit (MkHetQNonpolarPartialGroundedType $ ApplyNonpolarPartialGroundedType tf targ)
                        $ applyCoercion1 convf conva
