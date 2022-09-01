module Language.Expression.Dolan.Mono
    ( IsCovaryGroundType(..)
    , CovarySubtype(..)
    , nonpolarToMonoType
    ) where

import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Covariance
import Language.Expression.Dolan.Nonpolar
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

nonpolarToMonoType ::
       forall (ground :: GroundTypeKind) (conc :: forall k. k -> Type) a. CovarySubtype ground conc
    => NonpolarDolanType ground a
    -> Maybe (MonoType conc a)
nonpolarToMonoType (GroundedNonpolarType gt args) = do
    (lc, ct) <- dolanToMonoGroundType gt
    eargs <- ccrArgumentsToArgumentsM (\(CoNonpolarArgument arg) -> nonpolarToMonoType arg) lc args
    return $ MkMonoType ct eargs
nonpolarToMonoType _ = Nothing
