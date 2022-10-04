{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Rename
    ( dolanNamespaceRename
    , dolanNamespaceRenameArguments
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

dolanNamespaceRenameArguments ::
       forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv) polarity m t.
       (IsDolanGroundType ground, Monad m, Is PolarityType polarity)
    => DolanVarianceMap dv gt
    -> DolanArguments dv (DolanType ground) gt polarity t
    -> VarNamespaceT (DolanTypeSystem ground) (RenamerT (DolanTypeSystem ground) m) (DolanArguments dv (DolanType ground) gt polarity t)
dolanNamespaceRenameArguments dvm args = do
    MkShimWit args' (MkPolarMap conv) <-
        mapDolanArgumentsM @_ @PEqual (\t -> fmap mkPolarShimWit $ dolanNamespaceRename t) dvm args
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
    -> VarNamespaceT (DolanTypeSystem ground) (RenamerT (DolanTypeSystem ground) m) t
dolanNamespaceRename = namespaceRename @(DolanTypeSystem ground)

instance forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity) =>
             NamespaceRenamable (DolanTypeSystem ground) (DolanGroundedType ground polarity t) where
    namespaceRename (MkDolanGroundedType gt args) = do
        args' <- dolanNamespaceRenameArguments (groundTypeVarianceMap gt) args
        return $ MkDolanGroundedType gt args'

instance forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity) =>
             NamespaceRenamable (DolanTypeSystem ground) (DolanSingularType ground polarity t) where
    namespaceRename (GroundedDolanSingularType t) = do
        t' <- dolanNamespaceRename t
        return $ GroundedDolanSingularType t'
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

typeNamesArguments ::
       forall (ground :: GroundTypeKind) polarity dv gt t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanVarianceMap dv gt
    -> DolanArguments dv (DolanType ground) gt polarity t
    -> [String]
typeNamesArguments dvm args =
    runIdentity $
    execWriterT $ mapDolanArgumentsM @_ @PEqual (\t -> tell (typeNames t) >> return (mkShimWit t)) dvm args

typeNamesGrounded ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanGroundedType ground polarity t
    -> [String]
typeNamesGrounded (MkDolanGroundedType gt args) = typeNamesArguments (groundTypeVarianceMap gt) args

typeNamesSingular ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity t
    -> [String]
typeNamesSingular (GroundedDolanSingularType t) = typeNamesGrounded t
typeNamesSingular (VarDolanSingularType var) = [uVarName var]
typeNamesSingular (RecursiveDolanSingularType var t) = uVarName var : typeNames t

typeNames ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity t
    -> [String]
typeNames NilDolanType = return []
typeNames (ConsDolanType t1 tr) = typeNamesSingular t1 <> typeNames tr

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => RenameTypeSystem (DolanTypeSystem ground) where
    type RenamerT (DolanTypeSystem ground) = VarRenamerT (DolanTypeSystem ground)
    type RenamerNamespaceT (DolanTypeSystem ground) = VarNamespaceT (DolanTypeSystem ground)
    renameNegWitness = dolanNamespaceRename @ground
    renamePosWitness = dolanNamespaceRename @ground
    typeNamesNegWitness = typeNames
    typeNamesPosWitness = typeNames
    renameNewFreeVar = do
        n <- renamerGenerateFree
        newUVar n $ \wit -> return $ MkNewVar (varDolanShimWit wit) (varDolanShimWit wit)
    namespace nr = runVarNamespaceT nr
    runRenamer = runVarRenamerT
    finalRenamer = finalVarRenamerT
