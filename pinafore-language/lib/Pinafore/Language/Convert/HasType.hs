module Pinafore.Language.Convert.HasType where

import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes

type HasPinaforeType :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Constraint
class HasPinaforeType dv t | t -> dv where
    toNonpolarType :: NonpolarShimWit PinaforeGroundType dv t
    default toNonpolarType :: HasPinaforeGroundedType dv t => NonpolarShimWit PinaforeGroundType dv t
    toNonpolarType = groundNonpolarShimWit $ toNonpolarGroundedType @_ @t

toPinaforeType ::
       forall polarity (t :: Type). Is PolarityType polarity
    => HasPinaforeType '[] t => PinaforeShimWit polarity t
toPinaforeType = nonpolarToDolanShimWit toNonpolarType

instance KnownSymbol name => HasPinaforeType '[] (Var name) where
    toNonpolarType = varNonpolarShimWit $ MkSymbolType @name

type HasPinaforeGroundedType :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Constraint
class (HasPinaforeType dv t, HasDolanVary dv t) => HasPinaforeGroundedType dv t | t -> dv where
    toNonpolarGroundedType :: NonpolarGroundedShimWit PinaforeGroundType dv t
    default toNonpolarGroundedType :: HasPinaforeGroundType dv t => NonpolarGroundedShimWit PinaforeGroundType dv t
    toNonpolarGroundedType = groundNonpolarGroundShimWit $ toGroundType @_ @t
    toPinaforeGroundedType ::
           forall polarity a. Is PolarityType polarity
        => DolanArguments dv PinaforeType t polarity a
        -> PinaforeSingularShimWit polarity a
    default toPinaforeGroundedType ::
        forall polarity a.
            (HasPinaforeGroundType dv t, Is PolarityType polarity) =>
                    DolanArguments dv PinaforeType t polarity a -> PinaforeSingularShimWit polarity a
    toPinaforeGroundedType args = mkShimWit $ GroundedDolanSingularType toGroundType args

type HasPinaforeGroundType :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Constraint
class (CoercibleKind (DolanVarianceKind dv), InKind t, HasPinaforeGroundedType dv t) => HasPinaforeGroundType dv t
    | t -> dv
    where
    toGroundType :: PinaforeGroundType dv t

type HasPinaforeArgumentType :: forall (sv :: Variance) -> VarianceKind sv -> Constraint
class InKind t => HasPinaforeArgumentType sv t | t -> sv where
    toNonpolarArgumentType :: NonpolarArgumentDolanShimWit PinaforeGroundType t
    toPinaforeArgumentType ::
           forall polarity. Is PolarityType polarity
        => ArgTypeF (PinaforePolyShim Type) sv PinaforeType polarity t

instance forall (t :: Type). HasPinaforeType '[] t => HasPinaforeArgumentType 'Covariance t where
    toNonpolarArgumentType = SingleNonpolarArgumentShimWit toNonpolarType
    toPinaforeArgumentType =
        case toPinaforeType @_ @t of
            MkShimWit ta conv -> MkArgTypeF ta conv

instance forall (t :: Type). HasPinaforeType '[] t => HasPinaforeArgumentType 'Contravariance t where
    toNonpolarArgumentType = SingleNonpolarArgumentShimWit toNonpolarType
    toPinaforeArgumentType ::
           forall polarity. Is PolarityType polarity
        => ArgTypeF (PinaforePolyShim Type) 'Contravariance PinaforeType polarity t
    toPinaforeArgumentType =
        invertPolarity @polarity $
        case toPinaforeType @_ @t of
            MkShimWit ta conv -> MkArgTypeF ta (MkCatDual $ uninvertPolarMap conv)

instance forall (p :: Type) (q :: Type). (HasPinaforeType '[] p, HasPinaforeType '[] q) =>
             HasPinaforeArgumentType 'Rangevariance '( p, q) where
    toNonpolarArgumentType = PairNonpolarArgumentShimWit toNonpolarType toNonpolarType
    toPinaforeArgumentType ::
           forall polarity. Is PolarityType polarity
        => ArgTypeF (PinaforePolyShim Type) 'Rangevariance PinaforeType polarity '( p, q)
    toPinaforeArgumentType =
        invertPolarity @polarity $
        case toPinaforeType @_ @p of
            MkShimWit tp convp ->
                case toPinaforeType @_ @q of
                    MkShimWit tq convq -> MkArgTypeF (MkRangeType tp tq) $ MkCatRange (uninvertPolarMap convp) convq

instance forall sv dv (f :: DolanVarianceKind (sv ': dv)) (a :: VarianceKind sv). ( HasVariance sv f
         , Is DolanVarianceType dv
         , CoercibleKind (DolanVarianceKind dv)
         , HasDolanVary dv (f a)
         , HasPinaforeGroundedType (sv ': dv) f
         , HasPinaforeArgumentType sv a
         ) => HasPinaforeType dv (f a)

instance forall sv dv (f :: DolanVarianceKind (sv ': dv)) a. ( HasVariance sv f
         , Is DolanVarianceType dv
         , CoercibleKind (DolanVarianceKind dv)
         , HasDolanVary dv (f a)
         , HasPinaforeGroundedType (sv ': dv) f
         , HasPinaforeArgumentType sv a
         ) => HasPinaforeGroundedType dv (f a) where
    toNonpolarGroundedType =
        applyNonpolarGroundShimWit @PinaforeGroundType @sv toNonpolarGroundedType $ toNonpolarArgumentType @sv @a
    toPinaforeGroundedType ::
           forall polarity t. Is PolarityType polarity
        => DolanArguments dv PinaforeType (f a) polarity t
        -> PinaforeSingularShimWit polarity t
    toPinaforeGroundedType args =
        case dolanVary @(sv ': dv) @f of
            ConsDolanVarianceMap dvm ->
                case toPinaforeArgumentType @sv @a @polarity of
                    MkArgTypeF arg conv ->
                        case mapArgsTypeF (return . mkShimWit) representative dvm dvm args $
                             polarMapTypeApply (representative @_ @VarianceType @sv) cid conv of
                            Identity (MkShimWit args' conv') ->
                                mapShimWit conv' $
                                toPinaforeGroundedType @(sv ': dv) @f @polarity $ ConsDolanArguments arg args'

type ToPinaforeType = ToPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Positive)

type FromPinaforeType = FromPolarShimWit (PinaforePolyShim Type) (PinaforeType 'Negative)
