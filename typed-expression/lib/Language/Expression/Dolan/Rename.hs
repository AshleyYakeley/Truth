{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Rename
    ( dolanNamespaceRename
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

renameTypeArgs ::
       forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv) polarity m t.
       (IsDolanGroundType ground, Monad m, Is PolarityType polarity)
    => DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanArguments dv (DolanType ground) gt polarity t
    -> VarNamespaceT (DolanTypeSystem ground) (VarRenamerT (DolanTypeSystem ground) m) (DolanArguments dv (DolanType ground) gt polarity t)
renameTypeArgs dvt dvm args = do
    MkShimWit args' (MkPolarMap conv) <-
        mapDolanArgumentsM @_ @PEqual (\t -> fmap mkShimWit $ dolanNamespaceRename t) dvt dvm args
    return $
        case polarityType @polarity of
            PositiveType ->
                case conv of
                    MkPEqual -> args'
            NegativeType ->
                case conv of
                    MkPEqual -> args'

dolanNamespaceRename ::
       forall (ground :: GroundTypeKind) m t. (NamespaceRenamable (DolanTypeSystem ground) t, Monad m)
    => t
    -> VarNamespaceT (DolanTypeSystem ground) (VarRenamerT (DolanTypeSystem ground) m) t
dolanNamespaceRename = namespaceRename @(DolanTypeSystem ground)

instance forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity) =>
             NamespaceRenamable (DolanTypeSystem ground) (DolanSingularType ground polarity t) where
    namespaceRename (GroundDolanSingularType gt args) = do
        args' <- renameTypeArgs (groundTypeVarianceType gt) (groundTypeVarianceMap gt) args
        return $ GroundDolanSingularType gt args'
    namespaceRename (VarDolanSingularType oldvar) = do
        MkVarType newvar <- varNamespaceTRenameUVar @Type oldvar
        return $ VarDolanSingularType newvar
    namespaceRename (RecursiveDolanSingularType oldvar st) = do
        varNamespaceTLocalUVar @Type oldvar $ \(MkVarType newvar) -> do
            st' <- dolanNamespaceRename @ground st
            return $ RecursiveDolanSingularType newvar st'

instance forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity) =>
             NamespaceRenamable (DolanTypeSystem ground) (DolanType ground polarity t) where
    namespaceRename NilDolanType = return NilDolanType
    namespaceRename (ConsDolanType ta tb) = do
        ta' <- dolanNamespaceRename @ground ta
        tb' <- dolanNamespaceRename @ground tb
        return $ ConsDolanType ta' tb'

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => RenameTypeSystem (DolanTypeSystem ground) where
    type RenamerT (DolanTypeSystem ground) = VarRenamerT (DolanTypeSystem ground)
    type RenamerNamespaceT (DolanTypeSystem ground) = VarNamespaceT (DolanTypeSystem ground)
    renameNegWitness = dolanNamespaceRename @ground
    renamePosWitness = dolanNamespaceRename @ground
    renameNewVar = do
        n <- varRenamerTGenerate []
        newUVar n $ \wit -> return $ MkNewVar (varDolanShimWit wit) (varDolanShimWit wit)
    namespace = runVarNamespaceT
    runRenamer = runVarRenamerT
