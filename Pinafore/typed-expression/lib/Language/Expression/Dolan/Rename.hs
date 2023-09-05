{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Rename
    ( dolanArgumentsVarRename
    , dolanNamespaceRenameArguments
    , renameableVars
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
        ContraCCRPolarArgument p -> withInvertPolarity @polarity $ fmap ContraCCRPolarArgument $ unEndoM f p
        RangeCCRPolarArgument p q ->
            withInvertPolarity @polarity $ liftA2 RangeCCRPolarArgument (unEndoM f p) (unEndoM f q)

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
dolanNamespaceRenameArguments = pureMapDolanArgumentsM $ namespaceRenameType @(DolanTypeSystem ground)

dolanArgumentsVarRename ::
       forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv) polarity m.
       (IsDolanGroundType ground, Monad m, Is PolarityType polarity)
    => RenameSource m
    -> EndoM' m (DolanArguments dv (DolanType ground) gt polarity)
dolanArgumentsVarRename ev = pureMapDolanArgumentsM $ varRename ev

instance forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity) =>
             VarRenameable (DolanGroundedType ground polarity t) where
    varRename ev =
        MkEndoM $ \(MkDolanGroundedType gt args) -> do
            args' <- unEndoM (dolanArgumentsVarRename ev) args
            return $ MkDolanGroundedType gt args'

instance forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity) =>
             VarRenameable (DolanSingularType ground polarity t) where
    varRename ev =
        MkEndoM $ \case
            GroundedDolanSingularType t -> do
                t' <- unEndoM (varRename ev) t
                return $ GroundedDolanSingularType t'
            VarDolanSingularType oldvar -> do
                newvar <- unEndoM (varRename ev) oldvar
                return $ VarDolanSingularType newvar
            RecursiveDolanSingularType var st -> do
                var' <- unEndoM (rsNewVar ev) var
                st' <- unEndoM (varRename $ addToRenameSource var var' ev) st
                return $ RecursiveDolanSingularType var' st'

instance forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity) =>
             VarRenameable (DolanType ground polarity t) where
    varRename ev =
        MkEndoM $ \case
            NilDolanType -> pure NilDolanType
            ConsDolanType ta tb -> do
                ta' <- unEndoM (varRename ev) ta
                tb' <- unEndoM (varRename ev) tb
                return $ ConsDolanType ta' tb'

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => RenameTypeSystem (DolanTypeSystem ground) where
    type RenamerT (DolanTypeSystem ground) = VarRenamerT (DolanTypeSystem ground)
    type RenamerNamespaceT (DolanTypeSystem ground) = VarNamespaceT (DolanTypeSystem ground)
    namespaceRenameSource = varNamespaceRenameSource
    renameNewFreeVar = do
        n <- renamerGenerateFree
        newTypeVar n $ \v -> return $ MkNewVar (varDolanShimWit v) (varDolanShimWit v)
    namespace fn rgd = runVarNamespaceT fn rgd
    runRenamer = runVarRenamerT
    finalRenamer = finalVarRenamerT
