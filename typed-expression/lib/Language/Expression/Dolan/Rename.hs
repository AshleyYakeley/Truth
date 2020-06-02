{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Rename
    (
    ) where

import Data.Shim
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Language.Expression.Renamer
import Language.Expression.UVar
import Shapes

type TypeNamespace :: forall k. MapKind k -> Type -> (Polarity -> k -> Type) -> Type
type TypeNamespace cat ts w
     = forall t m polarity.
           (Monad m, Is PolarityType polarity) =>
                   Proxy polarity -> w polarity t -> VarNamespaceT ts (VarRenamerT ts m) (PShimWit cat w polarity t)

type DolanTypeNamespace :: forall k. GroundTypeKind -> (Polarity -> k -> Type) -> Type
type DolanTypeNamespace ground (w :: Polarity -> k -> Type)
     = TypeNamespace (PolyIso (DolanPolyShim ground) k) (DolanTypeSystem ground) w

renameTypeArgs ::
       forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv). (IsDolanGroundType ground)
    => DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanTypeNamespace ground (DolanArguments dv (DolanType ground) gt)
renameTypeArgs dvt dvm _ args =
    mapDolanArgumentsM @_ @(PolyIso (DolanPolyShim ground)) (renamePinaforeTypeVars Proxy) dvt dvm args

renamePinaforeSingularTypeVars ::
       forall (ground :: GroundTypeKind). (IsDolanGroundType ground)
    => DolanTypeNamespace ground (DolanSingularType ground)
renamePinaforeSingularTypeVars proxy (GroundDolanSingularType gt args) = do
    MkShimWit args' bij <- renameTypeArgs (groundTypeVarianceType gt) (groundTypeVarianceMap gt) proxy args
    return $ MkShimWit (GroundDolanSingularType gt args') bij
renamePinaforeSingularTypeVars (_ :: _ polarity) (VarDolanSingularType namewit1) =
    renameUVar @_ @(DolanPolyShim ground Type) varNamespaceTRename namewit1 $ \namewit2 bij ->
        return $
        MkShimWit (VarDolanSingularType namewit2) $
        MkPolarMap $
        case polarityType @polarity of
            PositiveType -> MkPolyMapT bij
            NegativeType -> MkPolyMapT $ cinvert bij

renamePinaforeTypeVars ::
       forall (ground :: GroundTypeKind). (IsDolanGroundType ground)
    => DolanTypeNamespace ground (DolanType ground)
renamePinaforeTypeVars (_ :: _ polarity) NilDolanType =
    return $
    case polarityType @polarity of
        PositiveType -> MkShimWit NilDolanType cid
        NegativeType -> MkShimWit NilDolanType cid
renamePinaforeTypeVars _ (ConsDolanType ta tb) = do
    MkShimWit ta' bija <- renamePinaforeSingularTypeVars Proxy ta
    MkShimWit tb' bijb <- renamePinaforeTypeVars Proxy tb
    return $ MkShimWit (ConsDolanType ta' tb') $ polarPolyIsoBimap bija bijb

instance forall (ground :: GroundTypeKind). (IsDolanGroundType ground) => Renamer (VarRenamerT (DolanTypeSystem ground)) where
    type RenamerNamespaceT (VarRenamerT (DolanTypeSystem ground)) = VarNamespaceT (DolanTypeSystem ground)
    type RenamerNegWitness (VarRenamerT (DolanTypeSystem ground)) = DolanType ground 'Negative
    type RenamerPosWitness (VarRenamerT (DolanTypeSystem ground)) = DolanType ground 'Positive
    type RenamerShim (VarRenamerT (DolanTypeSystem ground)) = DolanPolyShim ground Type
    renameNegWitness t = do
        MkShimWit t' bij <- renamePinaforeTypeVars Proxy t
        return $ MkShimWit t' $ polarPolyIsoForwards bij
    renamePosWitness t = do
        MkShimWit t' bij <- renamePinaforeTypeVars Proxy t
        return $ MkShimWit t' $ polarPolyIsoForwards bij
    renameNewVar = do
        n <- varRenamerTGenerate
        valueToWitness n $ \wit ->
            return $
            MkNewVar
                (singleDolanShimWit $ mkShimWit $ VarDolanSingularType wit)
                (singleDolanShimWit $ mkShimWit $ VarDolanSingularType wit)
    namespace = runVarNamespaceT
    runRenamer = runVarRenamerT
