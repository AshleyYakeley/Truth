module Language.Expression.TypeSystem.Rename.VarRenameable where

import Data.Shim
import Shapes

import Language.Expression.TypeSystem.TypeVariable

data RenameSource m = MkRenameSource
    { rsNewVar :: EndoM' m TypeVarT
    , rsRenameVar :: EndoM' m TypeVarT
    }

addToRenameSource ::
    forall m v.
    Monad m =>
    TypeVarT v ->
    TypeVarT v ->
    RenameSource m ->
    RenameSource m
addToRenameSource oldvar newvar (MkRenameSource nv rv) = let
    rv' :: EndoM' m TypeVarT
    rv' =
        MkEndoM $ \var ->
            case testEquality var oldvar of
                Just Refl -> pure newvar
                Nothing -> unEndoM rv var
    in MkRenameSource nv rv'

type VarRenameable :: Type -> Constraint
class VarRenameable t where
    varRename ::
        forall m.
        Monad m =>
        RenameSource m ->
        EndoM m t

renameableVars :: VarRenameable t => t -> [String]
renameableVars =
    execEndoMWriter
        $ varRename
        $ MkRenameSource{rsNewVar = mempty, rsRenameVar = MkEndoM $ \v -> tell [typeVarName v] >> return v}

instance (forall t'. VarRenameable (w t')) => VarRenameable (ShimWit shim w t) where
    varRename rs = endoShimWit $ varRename rs

instance VarRenameable (TypeVarT t) where
    varRename rs = rsRenameVar rs

instance VarRenameable (a :~: b) where
    varRename _ = mempty

-- hack to work around odd QualfifiedConstraints restriction
class (forall t. VarRenameable (w t)) => VarRenameable1 (w :: Type -> Type)

instance (forall t. VarRenameable (w t)) => VarRenameable1 (w :: Type -> Type)
