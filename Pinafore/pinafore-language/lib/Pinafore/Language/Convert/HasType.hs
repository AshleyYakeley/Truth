{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert.HasType where

import Import
import Pinafore.Language.Shim
import Pinafore.Language.Type

type HasQType :: Polarity -> Type -> Constraint
class Is PolarityType polarity => HasQType polarity t where
    qType :: QShimWit polarity t

groundQType :: (Is PolarityType polarity, HasQGroundType '[] t) => QShimWit polarity t
groundQType = shimWitToDolan $ mkShimWit $ MkDolanGroundedType qGroundType NilCCRArguments

instance {-# OVERLAPPABLE #-} forall polarity t. (Is PolarityType polarity, HasQGroundedType polarity t) =>
                                  HasQType polarity t where
    qType = shimWitToDolan $ qGroundedType @polarity @t

type HetQGroundedType :: Polarity -> CCRVariances -> forall k. k -> Type
data HetQGroundedType polarity dv f where
    MkHetQGroundedType
        :: forall polarity dv (f :: CCRVariancesKind dv). HasCCRVariances dv f
        => (forall a. CCRPolarArguments dv QType f polarity a -> QGroundedShimWit polarity a)
        -> HetQGroundedType polarity dv f

runHetQGroundedType ::
       forall polarity dv (f :: CCRVariancesKind dv) a.
       HetQGroundedType polarity dv f
    -> CCRPolarArguments dv QType f polarity a
    -> QGroundedShimWit polarity a
runHetQGroundedType (MkHetQGroundedType f) = f

type HetCCRVariancesOf :: forall k. k -> CCRVariances
type family HetCCRVariancesOf f where
    HetCCRVariancesOf (f :: Type) = '[]
    HetCCRVariancesOf (f :: Type -> k) = 'SimpleCCRVariance (VarianceOf f) ': HetCCRVariancesOf (f ())
    HetCCRVariancesOf (f :: (Type, Type) -> k) = 'RangeCCRVariance ': HetCCRVariancesOf (f '( (), ()))

type HasHetQGroundedType :: Polarity -> forall k. k -> Constraint
class Is PolarityType polarity => HasHetQGroundedType polarity (f :: k) where
    hetQGroundedType :: HetQGroundedType polarity (HetCCRVariancesOf f) f

instance {-# OVERLAPPABLE #-} forall polarity k (f :: k). ( Is PolarityType polarity
                              , HetConstraint (HasQGroundType (HetCCRVariancesOf f)) f
                              ) => HasHetQGroundedType polarity f where
    hetQGroundedType =
        case hetConstraint @_ @(HasQGroundType (HetCCRVariancesOf f)) @_ @f of
            MkHetConstraintWitness -> MkHetQGroundedType $ \args -> mkShimWit $ MkDolanGroundedType qGroundType args

type HasQGroundedType (polarity :: Polarity) (t :: Type) = HasHetQGroundedType polarity t

qGroundedType ::
       forall polarity t. HasQGroundedType polarity t
    => QGroundedShimWit polarity t
qGroundedType = runHetQGroundedType (hetQGroundedType @polarity @_ @t) NilCCRArguments

type HasQGroundType :: forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Constraint
class (CoercibleKind (CCRVariancesKind dv), dv ~ HetCCRVariancesOf f, HasCCRVariances dv f) => HasQGroundType dv f
    | f -> dv
    where
    qGroundType :: QGroundType dv f

type HasQArgumentType :: Polarity -> forall (sv :: CCRVariance) -> CCRVarianceKind sv -> Constraint
class Is PolarityType polarity => HasQArgumentType polarity sv t | t -> sv where
    qArgumentType :: CCRPolarArgumentShimWit (QPolyShim Type) QType polarity sv t

instance forall polarity (t :: Type). HasQType polarity t => HasQArgumentType polarity CoCCRVariance t where
    qArgumentType =
        case qType @polarity @t of
            MkShimWit ta conv -> MkShimWit (CoCCRPolarArgument ta) conv

instance forall polarity (t :: Type). (Is PolarityType polarity, HasQType (InvertPolarity polarity) t) =>
             HasQArgumentType polarity ContraCCRVariance t where
    qArgumentType :: CCRPolarArgumentShimWit (QPolyShim Type) QType polarity ContraCCRVariance t
    qArgumentType =
        withInvertPolarity @polarity $
        case qType @(InvertPolarity polarity) @t of
            MkShimWit ta conv -> MkShimWit (ContraCCRPolarArgument ta) (MkCatDual $ uninvertPolarShim conv)

instance forall polarity (p :: Type) (q :: Type). (HasQType (InvertPolarity polarity) p, HasQType polarity q) =>
             HasQArgumentType polarity 'RangeCCRVariance '( p, q) where
    qArgumentType :: CCRPolarArgumentShimWit (QPolyShim Type) QType polarity 'RangeCCRVariance '( p, q)
    qArgumentType =
        withInvertPolarity @polarity $
        case qType @(InvertPolarity polarity) @p of
            MkShimWit tp convp ->
                case qType @polarity @q of
                    MkShimWit tq convq ->
                        MkShimWit (RangeCCRPolarArgument tp tq) $ MkCatRange (uninvertPolarShim convp) convq

instance forall dv k (f :: Type -> k) polarity (a :: Type). ( HasVariance f
         , Is CCRVariancesType dv
         , k ~ CCRVariancesKind dv
         , CoercibleKind (CCRVariancesKind dv)
         , HetConstraint (HasCCRVariances dv) (f a)
         , HetCCRVariancesOf f ~ ('SimpleCCRVariance (VarianceOf f) ': dv)
         , HasHetQGroundedType polarity f
         , HetCCRVariancesOf (f a) ~ dv
         , HasQArgumentType polarity ('SimpleCCRVariance (VarianceOf f)) a
         , Is PolarityType polarity
         ) => HasHetQGroundedType polarity (f a) where
    hetQGroundedType =
        case hetQGroundedType @polarity @_ @f of
            MkHetQGroundedType toConvertibleType ->
                case hetConstraint @_ @(HasCCRVariances dv) @_ @(f a) of
                    MkHetConstraintWitness -> let
                        hetQGroundedType' ::
                               forall t. CCRPolarArguments dv QType (f a) polarity t -> QGroundedShimWit polarity t
                        hetQGroundedType' args =
                            case ccrVariancesMap @('SimpleCCRVariance (VarianceOf f) ': dv) @f of
                                ConsCCRVariancesMap ccrv dvm ->
                                    case qArgumentType @polarity @('SimpleCCRVariance (VarianceOf f)) @a of
                                        MkShimWit arg conv ->
                                            case mapDolanArgumentsFM (return . mkShimWit) dvm dvm args $
                                                 polarShimTypeApply
                                                     (representative
                                                          @_
                                                          @CCRVarianceType
                                                          @('SimpleCCRVariance (VarianceOf f)))
                                                     ccrv
                                                     ccrv
                                                     id
                                                     conv of
                                                Identity (MkShimWit args' conv') ->
                                                    mapShimWit conv' $ toConvertibleType $ ConsCCRArguments arg args'
                        in MkHetQGroundedType hetQGroundedType'

instance forall dv k (f :: (Type, Type) -> k) polarity (a :: (Type, Type)). ( HasCCRVariance 'RangeCCRVariance f
         , Is CCRVariancesType dv
         , CoercibleKind (CCRVariancesKind dv)
         , HetConstraint (HasCCRVariances dv) (f a)
         , HetCCRVariancesOf f ~ ('RangeCCRVariance ': dv)
         , HasHetQGroundedType polarity f
         , HasQArgumentType polarity 'RangeCCRVariance a
         , Is PolarityType polarity
         , HetCCRVariancesOf (f a) ~ dv
         ) => HasHetQGroundedType polarity (f a) where
    hetQGroundedType =
        case hetQGroundedType @polarity @_ @f of
            MkHetQGroundedType toConvertibleType ->
                case hetConstraint @_ @(HasCCRVariances dv) @_ @(f a) of
                    MkHetConstraintWitness -> let
                        hetQGroundedType' ::
                               forall t. CCRPolarArguments dv QType (f a) polarity t -> QGroundedShimWit polarity t
                        hetQGroundedType' args =
                            case ccrVariancesMap @('RangeCCRVariance ': dv) @f of
                                ConsCCRVariancesMap ccrv dvm ->
                                    case qArgumentType @polarity @'RangeCCRVariance @a of
                                        MkShimWit arg conv ->
                                            case mapDolanArgumentsFM (return . mkShimWit) dvm dvm args $
                                                 polarShimTypeApply
                                                     (representative @_ @CCRVarianceType @'RangeCCRVariance)
                                                     ccrv
                                                     ccrv
                                                     id
                                                     conv of
                                                Identity (MkShimWit args' conv') ->
                                                    mapShimWit conv' $ toConvertibleType $ ConsCCRArguments arg args'
                        in MkHetQGroundedType hetQGroundedType'

instance HasQType 'Positive t => ToPolarShimWit (QPolyShim Type) (QType 'Positive) t where
    toPolarShimWit = qType

instance HasQType 'Negative t => FromPolarShimWit (QPolyShim Type) (QType 'Negative) t where
    fromPolarShimWit = qType
