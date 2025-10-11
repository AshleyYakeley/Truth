{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-overlapping-patterns #-}

module Pinafore.Language.Convert.HasType where

import Language.Expression.Dolan

import Import
import Pinafore.Language.Type

type HasQType :: PolyShimKind -> Polarity -> Type -> Constraint
class (CCRVariancesShim pshim, Is PolarityType polarity) => HasQType pshim polarity t where
    qType :: PShimWit (pshim Type) QType polarity t

groundQType :: (Is PolarityType polarity, HasQGroundType '[] t) => QShimWit polarity t
groundQType = shimWitToDolan $ mkShimWit $ MkDolanGroundedType qGroundType NilCCRArguments

instance
    {-# OVERLAPPABLE #-}
    forall (pshim :: PolyShimKind) polarity t.
    ( CCRVariancesShim pshim
    , JoinMeetIsoShim (pshim Type)
    , Is PolarityType polarity
    , HasQGroundedType pshim polarity t
    ) =>
    HasQType pshim polarity t
    where
    qType = shimWitToDolan $ qGroundedType @pshim @polarity @t

type HetCCRVariancesOf :: forall k. k -> CCRVariances
type family HetCCRVariancesOf f where
    HetCCRVariancesOf (_ :: Type) = '[]
    HetCCRVariancesOf (f :: Type -> _) = 'SimpleCCRVariance (VarianceOf f) ': HetCCRVariancesOf (f ())
    HetCCRVariancesOf (f :: (Type, Type) -> _) = 'RangeCCRVariance ': HetCCRVariancesOf (f '((), ()))

type HasQGroundedType :: PolyShimKind -> Polarity -> Type -> Constraint
type HasQGroundedType pshim polarity = HasQPartialGroundedType pshim polarity

qGroundedType ::
    forall (pshim :: PolyShimKind) polarity t.
    HasQGroundedType pshim polarity t =>
    PShimWit (pshim Type) QGroundedType polarity t
qGroundedType = chainShimWit (\pgt -> mkShimWit $ partialToGroundedType pgt NilCCRArguments) $ qPartialGroundedType @pshim @polarity @'[] @t

type HasQGroundType :: forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Constraint
class
    (CoercibleKind (CCRVariancesKind dv), dv ~ HetCCRVariancesOf f, HasCCRVariances dv f) =>
    HasQGroundType dv f
        | f -> dv
    where
    qGroundType :: QGroundType dv f

qSomeGroundType ::
    forall dv (t :: CCRVariancesKind dv).
    HasQGroundType dv t =>
    QSomeGroundType
qSomeGroundType = MkSomeGroundType $ qGroundType @dv @t

type HasQArgumentType :: PolyShimKind -> Polarity -> forall (sv :: CCRVariance) -> CCRVarianceKind sv -> Constraint
class Is PolarityType polarity => HasQArgumentType pshim polarity sv t | t -> sv where
    qArgumentType :: CCRPolarArgumentShimWit (pshim Type) QType polarity sv t

instance
    forall (pshim :: PolyShimKind) polarity (t :: Type).
    HasQType pshim polarity t =>
    HasQArgumentType pshim polarity CoCCRVariance t
    where
    qArgumentType =
        case qType @pshim @polarity @t of
            MkShimWit ta conv -> MkShimWit (CoCCRPolarArgument ta) conv

instance
    forall (pshim :: PolyShimKind) polarity (t :: Type).
    ( Is PolarityType polarity
    , HasQType pshim (InvertPolarity polarity) t
    ) =>
    HasQArgumentType pshim polarity ContraCCRVariance t
    where
    qArgumentType :: CCRPolarArgumentShimWit (pshim Type) QType polarity ContraCCRVariance t
    qArgumentType =
        withInvertPolarity @polarity
            $ case qType @pshim @(InvertPolarity polarity) @t of
                MkShimWit ta conv -> MkShimWit (ContraCCRPolarArgument ta) (MkCatDual $ uninvertPolarShim conv)

instance
    forall (pshim :: PolyShimKind) polarity (p :: Type) (q :: Type).
    ( HasQType pshim (InvertPolarity polarity) p
    , HasQType pshim polarity q
    ) =>
    HasQArgumentType pshim polarity 'RangeCCRVariance '(p, q)
    where
    qArgumentType :: CCRPolarArgumentShimWit (pshim Type) QType polarity 'RangeCCRVariance '(p, q)
    qArgumentType =
        withInvertPolarity @polarity
            $ case qType @pshim @(InvertPolarity polarity) @p of
                MkShimWit tp convp ->
                    case qType @pshim @polarity @q of
                        MkShimWit tq convq ->
                            MkShimWit (RangeCCRPolarArgument tp tq) $ MkCatRange (uninvertPolarShim convp) convq

instance forall (pshim :: PolyShimKind) t. HasQType pshim 'Positive t => ToPolarShimWit (pshim Type) (QType 'Positive) t where
    toPolarShimWit = qType

instance
    forall (pshim :: PolyShimKind) t.
    HasQType pshim 'Negative t =>
    FromPolarShimWit (pshim Type) (QType 'Negative) t
    where
    fromPolarShimWit = qType

type HetQPartialGroundedType :: forall k. CCRVariances -> Polarity -> k -> Type
data HetQPartialGroundedType dv polarity t where
    MkHetQPartialGroundedType ::
        forall polarity dv (t :: CCRVariancesKind dv).
        QPartialGroundedType dv polarity t ->
        HetQPartialGroundedType dv polarity t

runHetQPartialGroundedType ::
    forall polarity dv (t :: CCRVariancesKind dv).
    HetQPartialGroundedType dv polarity t ->
    QPartialGroundedType dv polarity t
runHetQPartialGroundedType (MkHetQPartialGroundedType pgt) = pgt

type HasQPartialGroundedType :: PolyShimKind -> Polarity -> forall k. k -> Constraint
class
    (CCRVariancesShim pshim, Is PolarityType polarity) =>
    HasQPartialGroundedType (pshim :: PolyShimKind) (polarity :: Polarity) (t :: k)
    where
    hetQPartialGroundedType :: PShimWit (pshim k) (HetQPartialGroundedType (HetCCRVariancesOf t)) polarity t

qPartialGroundedType ::
    forall (pshim :: PolyShimKind) polarity dv (t :: CCRVariancesKind dv).
    (HasQPartialGroundedType pshim polarity t, CoercibleKind (CCRVariancesKind dv), dv ~ HetCCRVariancesOf t) =>
    PShimWit (pshim (CCRVariancesKind dv)) (QPartialGroundedType dv) polarity t
qPartialGroundedType = chainShimWit (mkShimWit . runHetQPartialGroundedType) hetQPartialGroundedType

instance
    {-# OVERLAPPABLE #-}
    forall (pshim :: PolyShimKind) polarity k (f :: k).
    ( Category (pshim k)
    , CCRVariancesShim pshim
    , Is PolarityType polarity
    , HetConstraint (HasQGroundType (HetCCRVariancesOf f)) f
    ) =>
    HasQPartialGroundedType pshim polarity f
    where
    hetQPartialGroundedType =
        case hetConstraint @_ @(HasQGroundType (HetCCRVariancesOf f)) @_ @f of
            MkHetConstraintWitness -> mkShimWit $ MkHetQPartialGroundedType $ GroundDolanPartialGroundedType qGroundType

instance
    forall (pshim :: PolyShimKind) dv k (f :: Type -> k) polarity (a :: Type).
    ( CCRVariancesShim pshim
    , HasVariance f
    , k ~ CCRVariancesKind dv
    , CoercibleKind k
    , HetCCRVariancesOf f ~ ('SimpleCCRVariance (VarianceOf f) ': dv)
    , HasQPartialGroundedType pshim polarity f
    , HetCCRVariancesOf (f a) ~ dv
    , HasQArgumentType pshim polarity ('SimpleCCRVariance (VarianceOf f)) a
    , Is PolarityType polarity
    ) =>
    HasQPartialGroundedType pshim polarity (f a)
    where
    hetQPartialGroundedType = case hetQPartialGroundedType @pshim @polarity @(Type -> k) @f of
        MkShimWit (MkHetQPartialGroundedType (tf :: _ f')) convf ->
            case qArgumentType @pshim @polarity @('SimpleCCRVariance (VarianceOf f)) @a of
                MkShimWit (targ :: _ a') convarg ->
                    MkShimWit (MkHetQPartialGroundedType $ ApplyDolanPartialGroundedType tf targ)
                        $ polarShimTypeApply
                            ( representative
                                @_
                                @CCRVarianceType
                                @('SimpleCCRVariance (VarianceOf f))
                            )
                            ccrVariation
                            (partialVariation tf)
                            convf
                            convarg

instance
    forall (pshim :: PolyShimKind) dv k (f :: (Type, Type) -> k) polarity (a :: (Type, Type)).
    ( CCRVariancesShim pshim
    , HasCCRVariance 'RangeCCRVariance f
    , k ~ CCRVariancesKind dv
    , CoercibleKind k
    , HetCCRVariancesOf f ~ ('RangeCCRVariance ': dv)
    , HasQPartialGroundedType pshim polarity f
    , HetCCRVariancesOf (f a) ~ dv
    , HasQArgumentType pshim polarity 'RangeCCRVariance a
    , Is PolarityType polarity
    ) =>
    HasQPartialGroundedType pshim polarity (f a)
    where
    hetQPartialGroundedType = case hetQPartialGroundedType @pshim @polarity @((Type, Type) -> k) @f of
        MkShimWit (MkHetQPartialGroundedType (tf :: _ f')) convf ->
            case qArgumentType @pshim @polarity @'RangeCCRVariance @a of
                MkShimWit (targ :: _ a') convarg ->
                    MkShimWit (MkHetQPartialGroundedType $ ApplyDolanPartialGroundedType tf targ)
                        $ polarShimTypeApply
                            ( representative
                                @_
                                @CCRVarianceType
                                @'RangeCCRVariance
                            )
                            ccrVariation
                            (partialVariation tf)
                            convf
                            convarg
