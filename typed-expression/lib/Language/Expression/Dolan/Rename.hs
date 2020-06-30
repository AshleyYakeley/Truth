{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Rename
    ( renameDolanIsoPlainType
    , renameDolanPlainType
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

type TypeNamespace :: forall k. ShimKind k -> Type -> (Polarity -> k -> Type) -> Type
type TypeNamespace shim ts w
     = forall t m polarity.
           (Monad m, Is PolarityType polarity) =>
                   Proxy polarity -> w polarity t -> VarNamespaceT ts (VarRenamerT ts m) (PShimWit shim w polarity t)

type DolanTypeNamespace :: forall k. GroundTypeKind -> (Polarity -> k -> Type) -> Type
type DolanTypeNamespace ground (w :: Polarity -> k -> Type)
     = TypeNamespace (DolanPolyIsoShim ground k) (DolanTypeSystem ground) w

renameTypeArgs ::
       forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv). (IsDolanGroundType ground)
    => DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanTypeNamespace ground (DolanArguments dv (DolanType ground) gt)
renameTypeArgs dvt dvm _ args = mapDolanArgumentsM @_ @(DolanPolyIsoShim ground) (renameTypeVars Proxy) dvt dvm args

renameSingularTypeVars ::
       forall (ground :: GroundTypeKind). (IsDolanGroundType ground)
    => DolanTypeNamespace ground (DolanSingularType ground)
renameSingularTypeVars proxy (GroundDolanSingularType gt args) = do
    MkShimWit args' bij <- renameTypeArgs (groundTypeVarianceType gt) (groundTypeVarianceMap gt) proxy args
    return $ MkShimWit (GroundDolanSingularType gt args') bij
renameSingularTypeVars (_ :: _ polarity) (VarDolanSingularType namewit1) = do
    newname <- varNamespaceTRename $ uVarName namewit1
    renameUVar @Type newname namewit1 $ \namewit2 -> return $ mkShimWit $ VarDolanSingularType namewit2

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
renameTypeVars pp@(_ :: _ polarity) (RecursiveDolanType namewit1 st) = do
    varNamespaceTLocal (uVarName namewit1) $ \newname -> do
        MkShimWit st' stmap <- renamePlainTypeVars pp st
        return $ MkShimWit (plainRecursiveDolanType newname st') stmap

renameDolanIsoPlainType ::
       forall (ground :: GroundTypeKind) polarity m t. (IsDolanGroundType ground, Is PolarityType polarity, Monad m)
    => DolanPlainType ground polarity t
    -> VarNamespaceT (DolanTypeSystem ground) (VarRenamerT (DolanTypeSystem ground) m) (DolanIsoPlainShimWit ground polarity t)
renameDolanIsoPlainType = renamePlainTypeVars Proxy

renameDolanPlainType ::
       forall (ground :: GroundTypeKind) polarity m t. (IsDolanGroundType ground, Is PolarityType polarity, Monad m)
    => DolanPlainType ground polarity t
    -> VarNamespaceT (DolanTypeSystem ground) (VarRenamerT (DolanTypeSystem ground) m) (DolanPlainShimWit ground polarity t)
renameDolanPlainType t = do
    MkShimWit t' bij <- renameDolanIsoPlainType t
    return $ MkShimWit t' $ polarPolyIsoForwards bij

renameDolanIsoType ::
       forall (ground :: GroundTypeKind) polarity m t. (IsDolanGroundType ground, Is PolarityType polarity, Monad m)
    => DolanType ground polarity t
    -> VarNamespaceT (DolanTypeSystem ground) (VarRenamerT (DolanTypeSystem ground) m) (DolanIsoShimWit ground polarity t)
renameDolanIsoType = renameTypeVars Proxy

renameDolanType ::
       forall (ground :: GroundTypeKind) polarity m t. (IsDolanGroundType ground, Is PolarityType polarity, Monad m)
    => DolanType ground polarity t
    -> VarNamespaceT (DolanTypeSystem ground) (VarRenamerT (DolanTypeSystem ground) m) (DolanShimWit ground polarity t)
renameDolanType t = do
    MkShimWit t' bij <- renameDolanIsoType t
    return $ MkShimWit t' $ polarPolyIsoForwards bij

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => RenameTypeSystem (DolanTypeSystem ground) where
    type RenamerT (DolanTypeSystem ground) = VarRenamerT (DolanTypeSystem ground)
    type RenamerNamespaceT (DolanTypeSystem ground) = VarNamespaceT (DolanTypeSystem ground)
    renameNegWitness = renameDolanType
    renamePosWitness = renameDolanType
    renameNewVar = do
        n <- varRenamerTGenerate []
        newUVar n $ \wit ->
            return $
            MkNewVar
                (singleDolanShimWit $ mkShimWit $ VarDolanSingularType wit)
                (singleDolanShimWit $ mkShimWit $ VarDolanSingularType wit)
    namespace = runVarNamespaceT
    runRenamer = runVarRenamerT
