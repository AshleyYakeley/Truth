{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert.HasType where

import Pinafore.Language.Shim
import Pinafore.Language.Type
import Shapes

type HasQType :: Polarity -> Type -> Constraint
class Is PolarityType polarity => HasQType polarity t where
    qType :: QShimWit polarity t

groundQType :: (Is PolarityType polarity, HasQGroundType '[] t) => QShimWit polarity t
groundQType = shimWitToDolan $ mkShimWit $ MkDolanGroundedType qGroundType NilCCRArguments

instance {-# OVERLAPPABLE #-} forall polarity (t :: Type). (Is PolarityType polarity, HasHetQGroundedType polarity t) =>
                                  HasQType polarity t where
    qType = shimWitToDolan $ runHetQGroundedType (hetQGroundedType @polarity @_ @t) NilCCRArguments

type HetQGroundedType :: Polarity -> DolanVariance -> forall k. k -> Type
data HetQGroundedType polarity dv f where
    MkHetQGroundedType
        :: forall polarity dv (f :: DolanVarianceKind dv). HasDolanVariance dv f
        => (forall a. DolanArguments dv QType f polarity a -> QGroundedShimWit polarity a)
        -> HetQGroundedType polarity dv f

runHetQGroundedType ::
       forall polarity dv (f :: DolanVarianceKind dv) a.
       HetQGroundedType polarity dv f
    -> DolanArguments dv QType f polarity a
    -> QGroundedShimWit polarity a
runHetQGroundedType (MkHetQGroundedType f) = f

type HetDolanVarianceOf :: forall k. k -> DolanVariance
type family HetDolanVarianceOf f where
    HetDolanVarianceOf (f :: Type) = '[]
    HetDolanVarianceOf (f :: Type -> k) = 'SimpleCCRVariance (VarianceOf f) ': HetDolanVarianceOf (f ())
    HetDolanVarianceOf (f :: (Type, Type) -> k) = 'RangeCCRVariance ': HetDolanVarianceOf (f '( (), ()))

type HasHetQGroundedType :: Polarity -> forall k. k -> Constraint
class Is PolarityType polarity => HasHetQGroundedType polarity (f :: k) where
    hetQGroundedType :: HetQGroundedType polarity (HetDolanVarianceOf f) f

instance {-# OVERLAPPABLE #-} forall polarity k (f :: k). ( Is PolarityType polarity
                              , HetConstraint (HasQGroundType (HetDolanVarianceOf f)) f
                              ) => HasHetQGroundedType polarity f where
    hetQGroundedType =
        case hetConstraint @_ @(HasQGroundType (HetDolanVarianceOf f)) @_ @f of
            MkHetConstraintWitness -> MkHetQGroundedType $ \args -> mkShimWit $ MkDolanGroundedType qGroundType args

type HasQGroundType :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Constraint
class (CoercibleKind (DolanVarianceKind dv), dv ~ HetDolanVarianceOf f, HasDolanVariance dv f) => HasQGroundType dv f
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
            MkShimWit ta conv -> MkShimWit (ContraCCRPolarArgument ta) (MkCatDual $ uninvertPolarMap conv)

instance forall polarity (p :: Type) (q :: Type). (HasQType (InvertPolarity polarity) p, HasQType polarity q) =>
             HasQArgumentType polarity 'RangeCCRVariance '( p, q) where
    qArgumentType :: CCRPolarArgumentShimWit (QPolyShim Type) QType polarity 'RangeCCRVariance '( p, q)
    qArgumentType =
        withInvertPolarity @polarity $
        case qType @(InvertPolarity polarity) @p of
            MkShimWit tp convp ->
                case qType @polarity @q of
                    MkShimWit tq convq ->
                        MkShimWit (RangeCCRPolarArgument tp tq) $ MkCatRange (uninvertPolarMap convp) convq

instance forall dv k (f :: Type -> k) polarity (a :: Type). ( HasVariance f
         , Is DolanVarianceType dv
         , k ~ DolanVarianceKind dv
         , CoercibleKind (DolanVarianceKind dv)
         , HetConstraint (HasDolanVariance dv) (f a)
         , HetDolanVarianceOf f ~ ('SimpleCCRVariance (VarianceOf f) ': dv)
         , HasHetQGroundedType polarity f
         , HetDolanVarianceOf (f a) ~ dv
         , HasQArgumentType polarity ('SimpleCCRVariance (VarianceOf f)) a
         , Is PolarityType polarity
         ) => HasHetQGroundedType polarity (f a) where
    hetQGroundedType =
        case hetQGroundedType @polarity @_ @f of
            MkHetQGroundedType toConvertibleType ->
                case hetConstraint @_ @(HasDolanVariance dv) @_ @(f a) of
                    MkHetConstraintWitness -> let
                        hetQGroundedType' ::
                               forall t. DolanArguments dv QType (f a) polarity t -> QGroundedShimWit polarity t
                        hetQGroundedType' args =
                            case dolanVarianceMap @('SimpleCCRVariance (VarianceOf f) ': dv) @f of
                                ConsDolanVarianceMap ccrv dvm ->
                                    case qArgumentType @polarity @('SimpleCCRVariance (VarianceOf f)) @a of
                                        MkShimWit arg conv ->
                                            case mapDolanArgumentsFM (return . mkShimWit) dvm dvm args $
                                                 polarMapTypeApply
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
         , Is DolanVarianceType dv
         , CoercibleKind (DolanVarianceKind dv)
         , HetConstraint (HasDolanVariance dv) (f a)
         , HetDolanVarianceOf f ~ ('RangeCCRVariance ': dv)
         , HasHetQGroundedType polarity f
         , HasQArgumentType polarity 'RangeCCRVariance a
         , Is PolarityType polarity
         , HetDolanVarianceOf (f a) ~ dv
         ) => HasHetQGroundedType polarity (f a) where
    hetQGroundedType =
        case hetQGroundedType @polarity @_ @f of
            MkHetQGroundedType toConvertibleType ->
                case hetConstraint @_ @(HasDolanVariance dv) @_ @(f a) of
                    MkHetConstraintWitness -> let
                        hetQGroundedType' ::
                               forall t. DolanArguments dv QType (f a) polarity t -> QGroundedShimWit polarity t
                        hetQGroundedType' args =
                            case dolanVarianceMap @('RangeCCRVariance ': dv) @f of
                                ConsDolanVarianceMap ccrv dvm ->
                                    case qArgumentType @polarity @'RangeCCRVariance @a of
                                        MkShimWit arg conv ->
                                            case mapDolanArgumentsFM (return . mkShimWit) dvm dvm args $
                                                 polarMapTypeApply
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
