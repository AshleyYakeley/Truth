{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Rename
    ( dolanArgumentsVarRename
    , pureMapDolanArgumentsM
    , renameableVars
    )
where

import Data.Shim
import Shapes

import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

pureMapDolanArgumentM ::
    forall m ft sv polarity.
    Applicative m =>
    (forall polarity'. Is PolarityType polarity' => EndoM' m (ft polarity')) ->
    (Is PolarityType polarity => EndoM' m (CCRPolarArgument ft polarity sv))
pureMapDolanArgumentM f =
    MkEndoM $ \case
        CoCCRPolarArgument q -> fmap CoCCRPolarArgument $ unEndoM f q
        ContraCCRPolarArgument p -> withInvertPolarity @polarity $ fmap ContraCCRPolarArgument $ unEndoM f p
        RangeCCRPolarArgument p q ->
            withInvertPolarity @polarity $ liftA2 RangeCCRPolarArgument (unEndoM f p) (unEndoM f q)

pureMapDolanArgumentsM ::
    forall m ft dv gt.
    Applicative m =>
    (forall polarity. Is PolarityType polarity => EndoM' m (ft polarity)) ->
    (forall polarity. Is PolarityType polarity => EndoM' m (CCRPolarArguments dv ft gt polarity))
pureMapDolanArgumentsM f =
    MkEndoM $ \case
        NilCCRArguments -> pure NilCCRArguments
        ConsCCRArguments arg args ->
            liftA2 ConsCCRArguments (unEndoM (pureMapDolanArgumentM f) arg) (unEndoM (pureMapDolanArgumentsM f) args)

dolanArgumentsVarRename ::
    forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv) polarity m.
    (IsDolanGroundType ground, Monad m, Is PolarityType polarity) =>
    RenameSource m ->
    EndoM' m (CCRPolarArguments dv (DolanType ground) gt polarity)
dolanArgumentsVarRename ev = pureMapDolanArgumentsM $ varRename ev

instance
    forall (ground :: GroundTypeKind) polarity dv t.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    VarRenameable (DolanPartialGroundedType ground dv polarity t)
    where
    varRename rs = MkEndoM $ \case
        GroundDolanPartialGroundedType gt -> return $ GroundDolanPartialGroundedType gt
        ApplyDolanPartialGroundedType pgt arg -> do
            pgt' <- unEndoM (varRename rs) pgt
            arg' <- unEndoM (pureMapDolanArgumentM $ varRename rs) arg
            return $ ApplyDolanPartialGroundedType pgt' arg'

instance
    forall (ground :: GroundTypeKind) polarity t.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    VarRenameable (DolanGroundedType ground polarity t)
    where
    varRename ev =
        MkEndoM $ \(MkDolanGroundedType gt args) -> do
            args' <- unEndoM (dolanArgumentsVarRename ev) args
            return $ MkDolanGroundedType gt args'

instance
    forall (ground :: GroundTypeKind) polarity t.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    VarRenameable (DolanSingularType ground polarity t)
    where
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

instance
    forall (ground :: GroundTypeKind) polarity t.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    VarRenameable (DolanType ground polarity t)
    where
    varRename ev =
        MkEndoM $ \case
            NilDolanType -> pure NilDolanType
            ConsDolanType ta tb -> do
                ta' <- unEndoM (varRename ev) ta
                tb' <- unEndoM (varRename ev) tb
                return $ ConsDolanType ta' tb'

instance
    forall (ground :: GroundTypeKind) polarity t.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    VarRenameable (InvertedType ground polarity t)
    where
    varRename ev = withInvertPolarity @polarity
        $ MkEndoM
        $ \case
            NilInvertedType -> pure NilInvertedType
            ConsInvertedType ta tb -> do
                ta' <- unEndoM (varRename ev) ta
                tb' <- unEndoM (varRename ev) tb
                return $ ConsInvertedType ta' tb'
