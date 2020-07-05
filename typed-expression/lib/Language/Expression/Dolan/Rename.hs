{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Rename
    ( renameDolanPlainType
    , renameDolanType
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

type Renamable :: GroundTypeKind -> Type -> Constraint
class Renamable ground t where
    renameIt ::
           forall m. Monad m
        => t
        -> VarNamespaceT (DolanTypeSystem ground) (VarRenamerT (DolanTypeSystem ground) m) t

renameTypeArgs ::
       forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv) polarity m t.
       (IsDolanGroundType ground, Monad m, Is PolarityType polarity)
    => DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> DolanArguments dv (DolanType ground) gt polarity t
    -> VarNamespaceT (DolanTypeSystem ground) (VarRenamerT (DolanTypeSystem ground) m) (DolanArguments dv (DolanType ground) gt polarity t)
renameTypeArgs dvt dvm args = do
    MkShimWit args' (MkPolarMap conv) <- mapDolanArgumentsM @_ @PEqual (\t -> fmap mkShimWit $ renameIt t) dvt dvm args
    return $
        case polarityType @polarity of
            PositiveType ->
                case conv of
                    MkPEqual -> args'
            NegativeType ->
                case conv of
                    MkPEqual -> args'

instance forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity) =>
             Renamable ground (DolanSingularType ground polarity t) where
    renameIt (GroundDolanSingularType gt args) = do
        args' <- renameTypeArgs (groundTypeVarianceType gt) (groundTypeVarianceMap gt) args
        return $ GroundDolanSingularType gt args'
    renameIt (VarDolanSingularType oldvar) = do
        MkAssignedUVar newvar <- varNamespaceTRenameUVar @Type oldvar
        return $ VarDolanSingularType newvar

instance forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity) =>
             Renamable ground (DolanPlainType ground polarity t) where
    renameIt NilDolanPlainType = return NilDolanPlainType
    renameIt (ConsDolanPlainType ta tb) = do
        ta' <- renameIt ta
        tb' <- renameIt tb
        return $ ConsDolanPlainType ta' tb'

instance forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity) =>
             Renamable ground (DolanType ground polarity t) where
    renameIt (PlainDolanType pt) = fmap PlainDolanType $ renameIt pt
    renameIt (RecursiveDolanType oldvar st) = do
        varNamespaceTLocalUVar @Type oldvar $ \(MkAssignedUVar newvar) -> do
            st' <- renameIt st
            return $ RecursiveDolanType newvar st'

renameDolanPlainType ::
       forall (ground :: GroundTypeKind) polarity m t. (IsDolanGroundType ground, Is PolarityType polarity, Monad m)
    => DolanPlainType ground polarity t
    -> VarNamespaceT (DolanTypeSystem ground) (VarRenamerT (DolanTypeSystem ground) m) (DolanPlainType ground polarity t)
renameDolanPlainType = renameIt

renameDolanType ::
       forall (ground :: GroundTypeKind) polarity m t. (IsDolanGroundType ground, Is PolarityType polarity, Monad m)
    => DolanType ground polarity t
    -> VarNamespaceT (DolanTypeSystem ground) (VarRenamerT (DolanTypeSystem ground) m) (DolanType ground polarity t)
renameDolanType = renameIt

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
