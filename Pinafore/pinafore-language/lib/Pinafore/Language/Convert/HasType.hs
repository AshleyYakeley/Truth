{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert.HasType where

import Pinafore.Language.Shim
import Pinafore.Language.Type
import Shapes

type HasPinaforeType :: Polarity -> Type -> Constraint
class Is PolarityType polarity => HasPinaforeType polarity t where
    pinaforeType :: PinaforeShimWit polarity t

groundPinaforeType :: (Is PolarityType polarity, HasPinaforeGroundType '[] t) => PinaforeShimWit polarity t
groundPinaforeType = singleDolanShimWit $ mkShimWit $ GroundedDolanSingularType pinaforeGroundType NilDolanArguments

instance {-# OVERLAPPABLE #-} forall polarity (t :: Type). ( Is PolarityType polarity
                              , HasHetPinaforeGroundedType polarity t
                              ) => HasPinaforeType polarity t where
    pinaforeType =
        singleDolanShimWit $ runHetPainforeGroundedType (hetPinaforeGroundedType @polarity @_ @t) NilDolanArguments

type HetPainforeGroundedType :: Polarity -> DolanVariance -> forall k. k -> Type
data HetPainforeGroundedType polarity dv f where
    MkHetPainforeGroundedType
        :: forall polarity dv (f :: DolanVarianceKind dv). HasDolanVariance dv f
        => (forall a. DolanArguments dv PinaforeType f polarity a -> PinaforeSingularShimWit polarity a)
        -> HetPainforeGroundedType polarity dv f

runHetPainforeGroundedType ::
       forall polarity dv (f :: DolanVarianceKind dv) a.
       HetPainforeGroundedType polarity dv f
    -> DolanArguments dv PinaforeType f polarity a
    -> PinaforeSingularShimWit polarity a
runHetPainforeGroundedType (MkHetPainforeGroundedType f) = f

type HetDolanVarianceOf :: forall k. k -> DolanVariance
type family HetDolanVarianceOf f where
    HetDolanVarianceOf (f :: Type) = '[]
    HetDolanVarianceOf (f :: Type -> k) = 'SimpleCCRVariance (VarianceOf f) ': HetDolanVarianceOf (f ())
    HetDolanVarianceOf (f :: (Type, Type) -> k) = 'RangeCCRVariance ': HetDolanVarianceOf (f '( (), ()))

type HasHetPinaforeGroundedType :: Polarity -> forall k. k -> Constraint
class Is PolarityType polarity => HasHetPinaforeGroundedType polarity (f :: k) where
    hetPinaforeGroundedType :: HetPainforeGroundedType polarity (HetDolanVarianceOf f) f

instance {-# OVERLAPPABLE #-} forall polarity k (f :: k). ( Is PolarityType polarity
                              , HetConstraint (HasPinaforeGroundType (HetDolanVarianceOf f)) f
                              ) => HasHetPinaforeGroundedType polarity f where
    hetPinaforeGroundedType =
        case hetConstraint @_ @(HasPinaforeGroundType (HetDolanVarianceOf f)) @_ @f of
            MkHetConstraintWitness ->
                MkHetPainforeGroundedType $ \args -> mkShimWit $ GroundedDolanSingularType pinaforeGroundType args

type HasPinaforeGroundType :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Constraint
class (CoercibleKind (DolanVarianceKind dv), InKind f, dv ~ HetDolanVarianceOf f, HasDolanVariance dv f) =>
          HasPinaforeGroundType dv f
    | f -> dv
    where
    pinaforeGroundType :: PinaforeGroundType dv f

type HasPinaforeArgumentType :: Polarity -> forall (sv :: CCRVariance) -> CCRVarianceKind sv -> Constraint
class (InKind t, Is PolarityType polarity) => HasPinaforeArgumentType polarity sv t | t -> sv where
    pinaforeArgumentType :: ArgTypeF (PinaforePolyShim Type) sv PinaforeType polarity t

instance forall polarity (t :: Type). HasPinaforeType polarity t => HasPinaforeArgumentType polarity CoCCRVariance t where
    pinaforeArgumentType =
        case pinaforeType @polarity @t of
            MkShimWit ta conv -> MkArgTypeF ta conv

instance forall polarity (t :: Type). (Is PolarityType polarity, HasPinaforeType (InvertPolarity polarity) t) =>
             HasPinaforeArgumentType polarity ContraCCRVariance t where
    pinaforeArgumentType :: ArgTypeF (PinaforePolyShim Type) ContraCCRVariance PinaforeType polarity t
    pinaforeArgumentType =
        invertPolarity @polarity $
        case pinaforeType @(InvertPolarity polarity) @t of
            MkShimWit ta conv -> MkArgTypeF ta (MkCatDual $ uninvertPolarMap conv)

instance forall polarity (p :: Type) (q :: Type). ( HasPinaforeType (InvertPolarity polarity) p
         , HasPinaforeType polarity q
         ) => HasPinaforeArgumentType polarity 'RangeCCRVariance '( p, q) where
    pinaforeArgumentType :: ArgTypeF (PinaforePolyShim Type) 'RangeCCRVariance PinaforeType polarity '( p, q)
    pinaforeArgumentType =
        invertPolarity @polarity $
        case pinaforeType @(InvertPolarity polarity) @p of
            MkShimWit tp convp ->
                case pinaforeType @polarity @q of
                    MkShimWit tq convq -> MkArgTypeF (MkRangeType tp tq) $ MkCatRange (uninvertPolarMap convp) convq

instance forall dv k (f :: Type -> k) polarity (a :: Type). ( HasVariance f
         , Is DolanVarianceType dv
         , k ~ DolanVarianceKind dv
         , CoercibleKind (DolanVarianceKind dv)
         , HetConstraint (HasDolanVariance dv) (f a)
         , HetDolanVarianceOf f ~ ('SimpleCCRVariance (VarianceOf f) ': dv)
         , HasHetPinaforeGroundedType polarity f
         , HetDolanVarianceOf (f a) ~ dv
         , HasPinaforeArgumentType polarity ('SimpleCCRVariance (VarianceOf f)) a
         , Is PolarityType polarity
         ) => HasHetPinaforeGroundedType polarity (f a) where
    hetPinaforeGroundedType =
        case hetPinaforeGroundedType @polarity @_ @f of
            MkHetPainforeGroundedType toConvertibleType ->
                case hetConstraint @_ @(HasDolanVariance dv) @_ @(f a) of
                    MkHetConstraintWitness -> let
                        hetPinaforeGroundedType' ::
                               forall t.
                               DolanArguments dv PinaforeType (f a) polarity t
                            -> PinaforeSingularShimWit polarity t
                        hetPinaforeGroundedType' args =
                            case dolanVarianceMap @('SimpleCCRVariance (VarianceOf f) ': dv) @f of
                                ConsDolanVarianceMap ccrv dvm ->
                                    case pinaforeArgumentType @polarity @('SimpleCCRVariance (VarianceOf f)) @a of
                                        MkArgTypeF arg conv ->
                                            case mapArgsTypeF (return . mkShimWit) representative dvm dvm args $
                                                 polarMapTypeApply
                                                     (representative
                                                          @_
                                                          @CCRVarianceType
                                                          @('SimpleCCRVariance (VarianceOf f)))
                                                     ccrv
                                                     ccrv
                                                     cid
                                                     conv of
                                                Identity (MkShimWit args' conv') ->
                                                    mapShimWit conv' $ toConvertibleType $ ConsDolanArguments arg args'
                        in MkHetPainforeGroundedType hetPinaforeGroundedType'

instance forall dv k (f :: (Type, Type) -> k) polarity (a :: (Type, Type)). ( HasCCRVariance 'RangeCCRVariance f
         , Is DolanVarianceType dv
         , CoercibleKind (DolanVarianceKind dv)
         , HetConstraint (HasDolanVariance dv) (f a)
         , HetDolanVarianceOf f ~ ('RangeCCRVariance ': dv)
         , HasHetPinaforeGroundedType polarity f
         , HasPinaforeArgumentType polarity 'RangeCCRVariance a
         , Is PolarityType polarity
         , HetDolanVarianceOf (f a) ~ dv
         ) => HasHetPinaforeGroundedType polarity (f a) where
    hetPinaforeGroundedType =
        case hetPinaforeGroundedType @polarity @_ @f of
            MkHetPainforeGroundedType toConvertibleType ->
                case hetConstraint @_ @(HasDolanVariance dv) @_ @(f a) of
                    MkHetConstraintWitness -> let
                        hetPinaforeGroundedType' ::
                               forall t.
                               DolanArguments dv PinaforeType (f a) polarity t
                            -> PinaforeSingularShimWit polarity t
                        hetPinaforeGroundedType' args =
                            case dolanVarianceMap @('RangeCCRVariance ': dv) @f of
                                ConsDolanVarianceMap ccrv dvm ->
                                    case pinaforeArgumentType @polarity @'RangeCCRVariance @a of
                                        MkArgTypeF arg conv ->
                                            case mapArgsTypeF (return . mkShimWit) representative dvm dvm args $
                                                 polarMapTypeApply
                                                     (representative @_ @CCRVarianceType @'RangeCCRVariance)
                                                     ccrv
                                                     ccrv
                                                     cid
                                                     conv of
                                                Identity (MkShimWit args' conv') ->
                                                    mapShimWit conv' $ toConvertibleType $ ConsDolanArguments arg args'
                        in MkHetPainforeGroundedType hetPinaforeGroundedType'

instance HasPinaforeType 'Positive t => ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) t where
    toPolarShimWit = pinaforeType

instance HasPinaforeType 'Negative t => FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) t where
    fromPolarShimWit = pinaforeType
