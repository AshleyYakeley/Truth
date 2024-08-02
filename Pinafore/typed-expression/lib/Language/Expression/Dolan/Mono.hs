module Language.Expression.Dolan.Mono
    ( IsCovaryGroundType(..)
    , CovarySubtype(..)
    , nonpolarToMonoType
    ) where

import Data.Shim
import Language.Expression.Dolan.Nonpolar
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

class IsCovaryGroundType (w :: forall k. k -> Type) where
    groundTypeCovaryType ::
           forall (k :: Type) (t :: k) r.
           w t
        -> (forall (dv :: CCRVariances). k ~ CCRVariancesKind dv => CovaryType dv -> r)
        -> r
    groundTypeCovaryMap :: forall k (t :: k). w t -> CovaryMap t

class (IsDolanGroundType ground, IsCovaryGroundType conc) =>
          CovarySubtype (ground :: GroundTypeKind) (conc :: forall k. k -> Type) where
    dolanToMonoGroundType :: forall dv t. ground dv t -> Maybe (CovaryType dv, conc t)

nonpolarGroundedToMonoType ::
       forall (ground :: GroundTypeKind) (conc :: forall k. k -> Type) a. CovarySubtype ground conc
    => NonpolarGroundedType ground a
    -> Maybe (MonoType conc a)
nonpolarGroundedToMonoType (MkNonpolarGroundedType gt args) = do
    (lc, ct) <- dolanToMonoGroundType gt
    eargs <- ccrArgumentsToArgumentsM (\(CoNonpolarArgument arg) -> nonpolarToMonoType arg) lc args
    return $ MkMonoType ct eargs

nonpolarToMonoType ::
       forall (ground :: GroundTypeKind) (conc :: forall k. k -> Type) a. CovarySubtype ground conc
    => NonpolarType ground a
    -> Maybe (MonoType conc a)
nonpolarToMonoType (GroundedNonpolarType t) = nonpolarGroundedToMonoType t
nonpolarToMonoType _ = Nothing
