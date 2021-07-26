module Language.Expression.Dolan.Mono
    ( IsCovaryGroundType(..)
    , CovarySubtype(..)
    , dolanToMonoSimpleType
    , dolanToMonoType
    , monoToMaybeNegativeDolanType
    , monoToPositiveDolanType
    , monoToDolanType
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Covariance
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
    dolanToMonoGroundType :: forall dv t. ground dv t -> Maybe (CovaryType dv, conc t)
    monoToDolanGroundType :: forall dv t. CovaryType dv -> conc t -> ground dv t

dolanToMonoArgs ::
       forall (ground :: GroundTypeKind) (conc :: forall k. k -> Type) (polarity :: Polarity) dv f t.
       (CovarySubtype ground conc, Is PolarityType polarity)
    => CovaryType dv
    -> CovaryMap f
    -> DolanArguments dv (DolanType ground) f polarity t
    -> Maybe (PolarShimWit (DolanPolyIsoShim ground Type) (Arguments (MonoType conc) f) polarity t)
dolanToMonoArgs = dolanArgumentsToArgumentsM dolanToMonoType

dolanToMonoSimpleType ::
       forall (ground :: GroundTypeKind) (conc :: forall k. k -> Type) (polarity :: Polarity) dv f a.
       (CovarySubtype ground conc, Is PolarityType polarity)
    => CovaryType dv
    -> conc f
    -> DolanArguments dv (DolanType ground) f polarity a
    -> Maybe (PolarShimWit (DolanPolyIsoShim ground Type) (MonoType conc) polarity a)
dolanToMonoSimpleType lc gt args = do
    MkShimWit eargs conv <- dolanToMonoArgs lc (groundTypeCovaryMap gt) args
    return $ MkShimWit (MkMonoType gt eargs) conv

dolanSingularToMonoArgs ::
       forall (ground :: GroundTypeKind) (conc :: forall k. k -> Type) polarity a.
       (CovarySubtype ground conc, Is PolarityType polarity)
    => DolanSingularType ground polarity a
    -> Maybe (PolarShimWit (DolanPolyIsoShim ground Type) (MonoType conc) polarity a)
dolanSingularToMonoArgs (GroundDolanSingularType dgt args)
    | Just (lc, gt) <- dolanToMonoGroundType dgt = dolanToMonoSimpleType lc gt args
dolanSingularToMonoArgs _ = Nothing

dolanToMonoType ::
       forall (ground :: GroundTypeKind) (conc :: forall k. k -> Type) polarity a.
       (CovarySubtype ground conc, Is PolarityType polarity)
    => DolanType ground polarity a
    -> Maybe (PolarShimWit (DolanPolyIsoShim ground Type) (MonoType conc) polarity a)
dolanToMonoType (ConsDolanType t NilDolanType) = do
    MkShimWit et conv <- dolanSingularToMonoArgs t
    return $ MkShimWit et $ conv <.> polarPolyIsoPolar1
dolanToMonoType _ = Nothing

monoToMaybeNegativeDolanType ::
       forall (ground :: GroundTypeKind) (conc :: forall k. k -> Type) t. CovarySubtype ground conc
    => MonoType conc t
    -> Maybe (DolanShimWit ground 'Negative t)
monoToMaybeNegativeDolanType (MkMonoType gt args) =
    groundTypeCovaryType gt $ \ct -> do
        MkShimWit dargs conv <- argumentsToDolanArgumentsM monoToMaybeNegativeDolanType ct (groundTypeCovaryMap gt) args
        return $ singleDolanShimWit $ MkShimWit (GroundDolanSingularType (monoToDolanGroundType ct gt) dargs) conv

monoToPositiveDolanType ::
       forall (ground :: GroundTypeKind) (conc :: forall k. k -> Type) t. CovarySubtype ground conc
    => MonoType conc t
    -> DolanShimWit ground 'Positive t
monoToPositiveDolanType (MkMonoType gt args) =
    groundTypeCovaryType gt $ \ct ->
        case argumentsToDolanArguments monoToPositiveDolanType ct (groundTypeCovaryMap gt) args of
            MkShimWit dargs conv ->
                singleDolanShimWit $ MkShimWit (GroundDolanSingularType (monoToDolanGroundType ct gt) dargs) conv

monoToDolanType ::
       forall (ground :: GroundTypeKind) (conc :: forall k. k -> Type) polarity t.
       (CovarySubtype ground conc, Is PolarityType polarity)
    => MonoType conc t
    -> Maybe (DolanShimWit ground polarity t)
monoToDolanType et =
    case polarityType @polarity of
        PositiveType -> return $ monoToPositiveDolanType et
        NegativeType -> monoToMaybeNegativeDolanType et
