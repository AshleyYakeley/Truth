{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Rename
    ( dolanNamespaceRename
    , dolanNamespaceRenameArguments
    , dolanNamespaceTypeNames
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Argument
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

pureMapDolanArgumentM ::
       forall m ft sv polarity. Applicative m
    => (forall polarity'. Is PolarityType polarity' => EndoM' m (ft polarity'))
    -> (Is PolarityType polarity => EndoM' m (CCRPolarArgument ft polarity sv))
pureMapDolanArgumentM f =
    MkEndoM $ \case
        CoCCRPolarArgument q -> fmap CoCCRPolarArgument $ unEndoM f q
        ContraCCRPolarArgument p -> invertPolarity @polarity $ fmap ContraCCRPolarArgument $ unEndoM f p
        RangeCCRPolarArgument p q -> invertPolarity @polarity $ liftA2 RangeCCRPolarArgument (unEndoM f p) (unEndoM f q)

pureMapDolanArgumentsM ::
       forall m ft dv gt. Applicative m
    => (forall polarity. Is PolarityType polarity => EndoM' m (ft polarity))
    -> (forall polarity. Is PolarityType polarity => EndoM' m (DolanArguments dv ft gt polarity))
pureMapDolanArgumentsM f =
    MkEndoM $ \case
        NilCCRArguments -> pure NilCCRArguments
        ConsCCRArguments arg args ->
            liftA2 ConsCCRArguments (unEndoM (pureMapDolanArgumentM f) arg) (unEndoM (pureMapDolanArgumentsM f) args)

dolanNamespaceRenameArguments ::
       forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv) polarity m.
       (IsDolanGroundType ground, Monad m, Is PolarityType polarity)
    => EndoM' (VarNamespaceT (DolanTypeSystem ground) (RenamerT (DolanTypeSystem ground) m)) (DolanArguments dv (DolanType ground) gt polarity)
dolanNamespaceRenameArguments = pureMapDolanArgumentsM dolanNamespaceRename

dolanNamespaceRename ::
       forall (ground :: GroundTypeKind) m t. (NamespaceRenamable (DolanTypeSystem ground) t, Monad m)
    => EndoM (VarNamespaceT (DolanTypeSystem ground) (RenamerT (DolanTypeSystem ground) m)) t
dolanNamespaceRename = namespaceRename @(DolanTypeSystem ground)

dolanNamespaceTypeNames ::
       forall (ground :: GroundTypeKind) t. (NamespaceRenamable (DolanTypeSystem ground) t)
    => t
    -> [String]
dolanNamespaceTypeNames = namespaceTypeNames @(DolanTypeSystem ground)

typeNamesArguments ::
       forall (ground :: GroundTypeKind) polarity dv gt t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanArguments dv (DolanType ground) gt polarity t
    -> [String]
typeNamesArguments = execEndoMWriter $ pureMapDolanArgumentsM $ tellEndoM $ dolanNamespaceTypeNames @ground

instance forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity) =>
             NamespaceRenamable (DolanTypeSystem ground) (DolanGroundedType ground polarity t) where
    namespaceRename =
        MkEndoM $ \(MkDolanGroundedType gt args) -> do
            args' <- unEndoM dolanNamespaceRenameArguments args
            return $ MkDolanGroundedType gt args'
    namespaceTypeNames (MkDolanGroundedType _ args) = typeNamesArguments args

instance forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity) =>
             NamespaceRenamable (DolanTypeSystem ground) (DolanSingularType ground polarity t) where
    namespaceRename =
        MkEndoM $ \case
            GroundedDolanSingularType t -> do
                t' <- unEndoM dolanNamespaceRename t
                return $ GroundedDolanSingularType t'
            VarDolanSingularType oldvar -> do
                MkVarType newvar <- varNamespaceTRenameUVar @Type oldvar
                return $ VarDolanSingularType newvar
            RecursiveDolanSingularType oldvar st -> do
                varNamespaceTLocalUVar @Type oldvar $ \(MkVarType newvar) -> do
                    st' <- unEndoM (dolanNamespaceRename @ground) st
                    return $ RecursiveDolanSingularType newvar st'
    namespaceTypeNames (GroundedDolanSingularType t) = dolanNamespaceTypeNames @ground t
    namespaceTypeNames (VarDolanSingularType var) = [uVarName var]
    namespaceTypeNames (RecursiveDolanSingularType var t) = uVarName var : dolanNamespaceTypeNames @ground t

instance forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity) =>
             NamespaceRenamable (DolanTypeSystem ground) (DolanType ground polarity t) where
    namespaceRename =
        MkEndoM $ \case
            NilDolanType -> return NilDolanType
            ConsDolanType ta tb -> do
                ta' <- unEndoM (dolanNamespaceRename @ground) ta
                tb' <- unEndoM (dolanNamespaceRename @ground) tb
                return $ ConsDolanType ta' tb'
    namespaceTypeNames NilDolanType = []
    namespaceTypeNames (ConsDolanType t1 tr) = dolanNamespaceTypeNames @ground t1 <> dolanNamespaceTypeNames @ground tr

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => RenameTypeSystem (DolanTypeSystem ground) where
    type RenamerT (DolanTypeSystem ground) = VarRenamerT (DolanTypeSystem ground)
    type RenamerNamespaceT (DolanTypeSystem ground) = VarNamespaceT (DolanTypeSystem ground)
    renameNegWitness = dolanNamespaceRename @ground
    renamePosWitness = dolanNamespaceRename @ground
    typeNamesNegWitness = dolanNamespaceTypeNames @ground
    typeNamesPosWitness = dolanNamespaceTypeNames @ground
    renameNewFreeVar = do
        n <- renamerGenerateFree
        newUVar n $ \wit -> return $ MkNewVar (varDolanShimWit wit) (varDolanShimWit wit)
    namespace nr = runVarNamespaceT nr
    runRenamer = runVarRenamerT
    finalRenamer = finalVarRenamerT
