module Language.Expression.Dolan.Concrete
    ( IsCovaryGroundType(..)
    , CovarySubtype(..)
    , dolanToConcreteSimpleType
    , dolanToConcreteType
    , concreteToMaybeNegativeDolanType
    , concreteToPositiveDolanType
    , concreteToDolanType
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Covariance
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

class IsCovaryGroundType (w :: forall k. k -> Type) where
    groundTypeCovaryType ::
           forall (k :: Type) (t :: k) r.
           w t
        -> (forall (dv :: DolanVariance). k ~ DolanVarianceKind dv => CovaryType dv -> r)
        -> r
    groundTypeCovaryMap :: forall k (t :: k). w t -> CovaryMap t

class (IsDolanGroundType ground, IsCovaryGroundType conc) =>
          CovarySubtype (ground :: GroundTypeKind) (conc :: forall k. k -> Type) where
    dolanToConcreteGroundType :: forall dv t. ground dv t -> Maybe (CovaryType dv, conc t)
    concreteToDolanGroundType :: forall dv t. CovaryType dv -> conc t -> ground dv t

dolanToConcreteArgs ::
       forall (ground :: GroundTypeKind) (conc :: forall k. k -> Type) (polarity :: Polarity) dv f t.
       (CovarySubtype ground conc, Is PolarityType polarity)
    => CovaryType dv
    -> CovaryMap f
    -> DolanArguments dv (DolanType ground) f polarity t
    -> Maybe (ShimWit (PolyIso (DolanPolyShim ground) Type) (Arguments (ConcreteType conc) f) polarity t)
dolanToConcreteArgs = dolanArgumentsToArgumentsM dolanToConcreteType

dolanToConcreteSimpleType ::
       forall (ground :: GroundTypeKind) (conc :: forall k. k -> Type) (polarity :: Polarity) dv f a.
       (CovarySubtype ground conc, Is PolarityType polarity)
    => CovaryType dv
    -> conc f
    -> DolanArguments dv (DolanType ground) f polarity a
    -> Maybe (ShimWit (PolyIso (DolanPolyShim ground) Type) (ConcreteType conc) polarity a)
dolanToConcreteSimpleType lc gt args = do
    MkShimWit eargs conv <- dolanToConcreteArgs lc (groundTypeCovaryMap gt) args
    return $ MkShimWit (MkConcreteType gt eargs) conv

dolanSingularToConcreteArgs ::
       forall (ground :: GroundTypeKind) (conc :: forall k. k -> Type) polarity a.
       (CovarySubtype ground conc, Is PolarityType polarity)
    => DolanSingularType ground polarity a
    -> Maybe (ShimWit (PolyIso (DolanPolyShim ground) Type) (ConcreteType conc) polarity a)
dolanSingularToConcreteArgs (GroundDolanSingularType dgt args)
    | Just (lc, gt) <- dolanToConcreteGroundType dgt = dolanToConcreteSimpleType lc gt args
dolanSingularToConcreteArgs _ = Nothing

dolanToConcreteType ::
       forall (ground :: GroundTypeKind) (conc :: forall k. k -> Type) polarity a.
       (CovarySubtype ground conc, Is PolarityType polarity)
    => DolanType ground polarity a
    -> Maybe (ShimWit (PolyIso (DolanPolyShim ground) Type) (ConcreteType conc) polarity a)
dolanToConcreteType (ConsDolanType t NilDolanType) = do
    MkShimWit et conv <- dolanSingularToConcreteArgs t
    return $ MkShimWit et $ conv <.> polarPolyIsoPolar1
dolanToConcreteType _ = Nothing

concreteToMaybeNegativeDolanType ::
       forall (ground :: GroundTypeKind) (conc :: forall k. k -> Type) t. CovarySubtype ground conc
    => ConcreteType conc t
    -> Maybe (PShimWit (DolanPolyShim ground Type) (DolanType ground) 'Negative t)
concreteToMaybeNegativeDolanType (MkConcreteType gt args) =
    groundTypeCovaryType gt $ \ct -> do
        MkShimWit dargs conv <-
            argumentsToDolanArgumentsM concreteToMaybeNegativeDolanType ct (groundTypeCovaryMap gt) args
        return $ singleDolanShimWit $ MkShimWit (GroundDolanSingularType (concreteToDolanGroundType ct gt) dargs) conv

concreteToPositiveDolanType ::
       forall (ground :: GroundTypeKind) (conc :: forall k. k -> Type) t. CovarySubtype ground conc
    => ConcreteType conc t
    -> PShimWit (DolanPolyShim ground Type) (DolanType ground) 'Positive t
concreteToPositiveDolanType (MkConcreteType gt args) =
    groundTypeCovaryType gt $ \ct ->
        case argumentsToDolanArguments concreteToPositiveDolanType ct (groundTypeCovaryMap gt) args of
            MkShimWit dargs conv ->
                singleDolanShimWit $ MkShimWit (GroundDolanSingularType (concreteToDolanGroundType ct gt) dargs) conv

concreteToDolanType ::
       forall (ground :: GroundTypeKind) (conc :: forall k. k -> Type) polarity t.
       (CovarySubtype ground conc, Is PolarityType polarity)
    => ConcreteType conc t
    -> Maybe (PShimWit (DolanPolyShim ground Type) (DolanType ground) polarity t)
concreteToDolanType et =
    case polarityType @polarity of
        PositiveType -> return $ concreteToPositiveDolanType et
        NegativeType -> concreteToMaybeNegativeDolanType et
