{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-overlapping-patterns #-}

module Pinafore.Language.Convert.HasType where

import Import
import Pinafore.Language.Type

type HasQType :: PolyShimKind -> Polarity -> Type -> Constraint
class (CCRVariancesShim pshim, Is PolarityType polarity) => HasQType pshim polarity t where
    qType :: PShimWit (pshim Type) QType polarity t

groundQType :: (Is PolarityType polarity, HasQGroundType '[] t) => QShimWit polarity t
groundQType = shimWitToDolan $ mkShimWit $ MkDolanGroundedType qGroundType NilCCRArguments

instance {-# OVERLAPPABLE #-} forall (pshim :: PolyShimKind) polarity t. ( CCRVariancesShim pshim
                              , JoinMeetIsoShim (pshim Type)
                              , Is PolarityType polarity
                              , HasQGroundedType pshim polarity t
                              ) => HasQType pshim polarity t where
    qType = shimWitToDolan $ qGroundedType @pshim @polarity @t

type HetQGroundedType :: PolyShimKind -> Polarity -> CCRVariances -> forall k. k -> Type
data HetQGroundedType pshim polarity dv f where
    MkHetQGroundedType
        :: forall (pshim :: PolyShimKind) polarity dv (f :: CCRVariancesKind dv). HasCCRVariances dv f
        => (forall a. CCRPolarArguments dv QType f polarity a -> PShimWit (pshim Type) QGroundedType polarity a)
        -> HetQGroundedType pshim polarity dv f

runHetQGroundedType ::
       forall (pshim :: PolyShimKind) polarity dv (f :: CCRVariancesKind dv) a.
       HetQGroundedType pshim polarity dv f
    -> CCRPolarArguments dv QType f polarity a
    -> PShimWit (pshim Type) QGroundedType polarity a
runHetQGroundedType (MkHetQGroundedType f) = f

type HetCCRVariancesOf :: forall k. k -> CCRVariances
type family HetCCRVariancesOf f where
    HetCCRVariancesOf (_ :: Type) = '[]
    HetCCRVariancesOf (f :: Type -> _) = 'SimpleCCRVariance (VarianceOf f) ': HetCCRVariancesOf (f ())
    HetCCRVariancesOf (f :: (Type, Type) -> _) = 'RangeCCRVariance ': HetCCRVariancesOf (f '( (), ()))

type HasHetQGroundedType :: PolyShimKind -> Polarity -> forall k. k -> Constraint
class Is PolarityType polarity => HasHetQGroundedType pshim polarity (f :: k) where
    hetQGroundedType :: HetQGroundedType pshim polarity (HetCCRVariancesOf f) f

instance {-# OVERLAPPABLE #-} forall (pshim :: PolyShimKind) polarity k (f :: k). ( Category (pshim Type)
                              , Is PolarityType polarity
                              , HetConstraint (HasQGroundType (HetCCRVariancesOf f)) f
                              ) => HasHetQGroundedType pshim polarity f where
    hetQGroundedType =
        case hetConstraint @_ @(HasQGroundType (HetCCRVariancesOf f)) @_ @f of
            MkHetConstraintWitness -> MkHetQGroundedType $ \args -> mkShimWit $ MkDolanGroundedType qGroundType args

type HasQGroundedType (pshim :: PolyShimKind) (polarity :: Polarity) (t :: Type) = HasHetQGroundedType pshim polarity t

qGroundedType ::
       forall (pshim :: PolyShimKind) polarity t. HasQGroundedType pshim polarity t
    => PShimWit (pshim Type) QGroundedType polarity t
qGroundedType = runHetQGroundedType (hetQGroundedType @pshim @polarity @_ @t) NilCCRArguments

type HasQGroundType :: forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Constraint
class (CoercibleKind (CCRVariancesKind dv), dv ~ HetCCRVariancesOf f, HasCCRVariances dv f) => HasQGroundType dv f
    | f -> dv
    where
    qGroundType :: QGroundType dv f

qSomeGroundType ::
       forall dv (t :: CCRVariancesKind dv). HasQGroundType dv t
    => QSomeGroundType
qSomeGroundType = MkSomeGroundType $ qGroundType @dv @t

type HasQArgumentType :: PolyShimKind -> Polarity -> forall (sv :: CCRVariance) -> CCRVarianceKind sv -> Constraint
class Is PolarityType polarity => HasQArgumentType pshim polarity sv t | t -> sv where
    qArgumentType :: CCRPolarArgumentShimWit (pshim Type) QType polarity sv t

instance forall (pshim :: PolyShimKind) polarity (t :: Type). HasQType pshim polarity t =>
             HasQArgumentType pshim polarity CoCCRVariance t where
    qArgumentType =
        case qType @pshim @polarity @t of
            MkShimWit ta conv -> MkShimWit (CoCCRPolarArgument ta) conv

instance forall (pshim :: PolyShimKind) polarity (t :: Type). ( Is PolarityType polarity
         , HasQType pshim (InvertPolarity polarity) t
         ) => HasQArgumentType pshim polarity ContraCCRVariance t where
    qArgumentType :: CCRPolarArgumentShimWit (pshim Type) QType polarity ContraCCRVariance t
    qArgumentType =
        withInvertPolarity @polarity $
        case qType @pshim @(InvertPolarity polarity) @t of
            MkShimWit ta conv -> MkShimWit (ContraCCRPolarArgument ta) (MkCatDual $ uninvertPolarShim conv)

instance forall (pshim :: PolyShimKind) polarity (p :: Type) (q :: Type). ( HasQType pshim (InvertPolarity polarity) p
         , HasQType pshim polarity q
         ) => HasQArgumentType pshim polarity 'RangeCCRVariance '( p, q) where
    qArgumentType :: CCRPolarArgumentShimWit (pshim Type) QType polarity 'RangeCCRVariance '( p, q)
    qArgumentType =
        withInvertPolarity @polarity $
        case qType @pshim @(InvertPolarity polarity) @p of
            MkShimWit tp convp ->
                case qType @pshim @polarity @q of
                    MkShimWit tq convq ->
                        MkShimWit (RangeCCRPolarArgument tp tq) $ MkCatRange (uninvertPolarShim convp) convq

instance forall (pshim :: PolyShimKind) dv k (f :: Type -> k) polarity (a :: Type). ( CCRVariancesShim pshim
         , HasVariance f
         , k ~ CCRVariancesKind dv
         , CoercibleKind (CCRVariancesKind dv)
         , HetConstraint (HasCCRVariances dv) (f a)
         , HetCCRVariancesOf f ~ ('SimpleCCRVariance (VarianceOf f) ': dv)
         , HasHetQGroundedType pshim polarity f
         , HetCCRVariancesOf (f a) ~ dv
         , HasQArgumentType pshim polarity ('SimpleCCRVariance (VarianceOf f)) a
         , Is PolarityType polarity
         ) => HasHetQGroundedType pshim polarity (f a) where
    hetQGroundedType =
        case hetQGroundedType @pshim @polarity @_ @f of
            MkHetQGroundedType toConvertibleType ->
                case hetConstraint @_ @(HasCCRVariances dv) @_ @(f a) of
                    MkHetConstraintWitness -> let
                        hetQGroundedType' ::
                               forall t.
                               CCRPolarArguments dv QType (f a) polarity t
                            -> PShimWit (pshim Type) QGroundedType polarity t
                        hetQGroundedType' args =
                            case ccrVariancesMap @('SimpleCCRVariance (VarianceOf f) ': dv) @f of
                                ConsCCRVariancesMap ccrv dvm ->
                                    case qArgumentType @pshim @polarity @('SimpleCCRVariance (VarianceOf f)) @a of
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

instance forall (pshim :: PolyShimKind) dv k (f :: (Type, Type) -> k) polarity (a :: (Type, Type)). ( CCRVariancesShim pshim
         , HasCCRVariance 'RangeCCRVariance f
         , CoercibleKind (CCRVariancesKind dv)
         , HetConstraint (HasCCRVariances dv) (f a)
         , HetCCRVariancesOf f ~ ('RangeCCRVariance ': dv)
         , HasHetQGroundedType pshim polarity f
         , HasQArgumentType pshim polarity 'RangeCCRVariance a
         , Is PolarityType polarity
         , HetCCRVariancesOf (f a) ~ dv
         ) => HasHetQGroundedType pshim polarity (f a) where
    hetQGroundedType =
        case hetQGroundedType @pshim @polarity @_ @f of
            MkHetQGroundedType toConvertibleType ->
                case hetConstraint @_ @(HasCCRVariances dv) @_ @(f a) of
                    MkHetConstraintWitness -> let
                        hetQGroundedType' ::
                               forall t.
                               CCRPolarArguments dv QType (f a) polarity t
                            -> PShimWit (pshim Type) QGroundedType polarity t
                        hetQGroundedType' args =
                            case ccrVariancesMap @('RangeCCRVariance ': dv) @f of
                                ConsCCRVariancesMap ccrv dvm ->
                                    case qArgumentType @pshim @polarity @'RangeCCRVariance @a of
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

instance forall (pshim :: PolyShimKind) t. HasQType pshim 'Positive t => ToPolarShimWit (pshim Type) (QType 'Positive) t where
    toPolarShimWit = qType

instance forall (pshim :: PolyShimKind) t. HasQType pshim 'Negative t =>
             FromPolarShimWit (pshim Type) (QType 'Negative) t where
    fromPolarShimWit = qType
