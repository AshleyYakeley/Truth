{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Rename
    ( renameDolanPlainType
    , renameDolanType
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Language.Expression.TypeVariable
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
    mapDolanArgumentsM @_ @(PolyIso (DolanPolyShim ground)) (renameTypeVars Proxy) dvt dvm args

renameSingularTypeVars ::
       forall (ground :: GroundTypeKind). (IsDolanGroundType ground)
    => DolanTypeNamespace ground (DolanSingularType ground)
renameSingularTypeVars proxy (GroundDolanSingularType gt args) = do
    MkShimWit args' bij <- renameTypeArgs (groundTypeVarianceType gt) (groundTypeVarianceMap gt) proxy args
    return $ MkShimWit (GroundDolanSingularType gt args') bij
renameSingularTypeVars (_ :: _ polarity) (VarDolanSingularType namewit1) =
    varNamespaceTRenameUVar @_ @_ @(DolanPolyShim ground Type) namewit1 $ \namewit2 bij ->
        return $ MkShimWit (VarDolanSingularType namewit2) $ isoPolyIso bij

renamePlainTypeVars ::
       forall (ground :: GroundTypeKind). (IsDolanGroundType ground)
    => DolanTypeNamespace ground (DolanPlainType ground)
renamePlainTypeVars (_ :: _ polarity) NilDolanPlainType = return $ mkShimWit NilDolanPlainType
renamePlainTypeVars _ (ConsDolanPlainType ta tb) = do
    MkShimWit ta' bija <- renameSingularTypeVars Proxy ta
    MkShimWit tb' bijb <- renamePlainTypeVars Proxy tb
    return $ MkShimWit (ConsDolanPlainType ta' tb') $ polarPolyIsoBimap bija bijb

renameTypeVars ::
       forall (ground :: GroundTypeKind). (IsDolanGroundType ground)
    => DolanTypeNamespace ground (DolanType ground)
renameTypeVars pp (PlainDolanType t) = fmap (chainShimWit $ mkShimWit . PlainDolanType) $ renamePlainTypeVars pp t
renameTypeVars pp@(_ :: _ polarity) (RecursiveDolanType namewit1 st) =
    varNamespaceTLocalUVar @_ @_ @(DolanPolyShim ground Type) namewit1 $ \namewit2 _ -> do
        MkShimWit st' stmap <- renamePlainTypeVars pp st
        return $ MkShimWit (RecursiveDolanType namewit2 st') stmap

renameDolanPlainType ::
       forall (ground :: GroundTypeKind) polarity m t. (IsDolanGroundType ground, Is PolarityType polarity, Monad m)
    => DolanPlainType ground polarity t
    -> VarNamespaceT (DolanTypeSystem ground) (VarRenamerT (DolanTypeSystem ground) m) (DolanPlainShimWit ground polarity t)
renameDolanPlainType t = do
    MkShimWit t' bij <- renamePlainTypeVars Proxy t
    return $ MkShimWit t' $ polarPolyIsoForwards bij

renameDolanType ::
       forall (ground :: GroundTypeKind) polarity m t. (IsDolanGroundType ground, Is PolarityType polarity, Monad m)
    => DolanType ground polarity t
    -> VarNamespaceT (DolanTypeSystem ground) (VarRenamerT (DolanTypeSystem ground) m) (DolanShimWit ground polarity t)
renameDolanType t = do
    MkShimWit t' bij <- renameTypeVars Proxy t
    return $ MkShimWit t' $ polarPolyIsoForwards bij

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => Renamer (VarRenamerT (DolanTypeSystem ground)) where
    type RenamerNamespaceT (VarRenamerT (DolanTypeSystem ground)) = VarNamespaceT (DolanTypeSystem ground)
    type RenamerNegWitness (VarRenamerT (DolanTypeSystem ground)) = DolanType ground 'Negative
    type RenamerPosWitness (VarRenamerT (DolanTypeSystem ground)) = DolanType ground 'Positive
    type RenamerShim (VarRenamerT (DolanTypeSystem ground)) = DolanPolyShim ground Type
    renameNegWitness = renameDolanType
    renamePosWitness = renameDolanType
    renameNewVar = do
        n <- varRenamerTGenerate []
        valueToWitness n $ \wit ->
            return $
            MkNewVar
                (singleDolanShimWit $ mkShimWit $ VarDolanSingularType wit)
                (singleDolanShimWit $ mkShimWit $ VarDolanSingularType wit)
    namespace = runVarNamespaceT
    runRenamer = runVarRenamerT
