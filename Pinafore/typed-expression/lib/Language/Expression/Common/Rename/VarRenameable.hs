module Language.Expression.Common.Rename.VarRenameable where

import Data.Shim
import Language.Expression.Common.Rename.RenameTypeSystem
import Language.Expression.Common.Rename.VarNamespaceT
import Language.Expression.Common.TypeVariable
import Shapes

type VarRenameable :: Type -> Constraint
class VarRenameable t where
    varRename ::
           forall m. Monad m
        => EndoM' m TypeVarT
        -> EndoM m t

renameableVars :: VarRenameable t => t -> [String]
renameableVars = execEndoMWriter $ varRename $ MkEndoM $ \v -> tell [typeVarName v] >> return v

namespaceRename ::
       forall ts m t. (RenameTypeSystem ts, RenamerMonad (RenamerT ts m), VarRenameable t, Monad m)
    => EndoM (VarNamespaceT ts (RenamerT ts m)) t
namespaceRename =
    case hasTransConstraint @Monad @(RenamerT ts) @m of
        Dict -> varRename $ MkEndoM $ \(MkTypeVar var) -> varNamespaceTRenameUVar @_ @_ @ts var

instance (forall t'. VarRenameable (w t')) => VarRenameable (PolarShimWit shim w polarity t) where
    varRename ev = endoShimWit $ varRename ev

instance VarRenameable (TypeVarT t) where
    varRename ev = ev
