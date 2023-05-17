module Language.Expression.Common.Rename.VarRenameable where

import Data.Shim
import Language.Expression.Common.TypeVariable
import Shapes

type VarRenameable :: Type -> Constraint
class VarRenameable t where
    varRename ::
           forall m. Monad m
        => EndoM' m TypeVarT
        -> EndoM m t

-- will actually included non-free recursively-quantified variables
renameableVars :: VarRenameable t => t -> [String]
renameableVars = execEndoMWriter $ varRename $ MkEndoM $ \v -> tell [typeVarName v] >> return v

instance (forall t'. VarRenameable (w t')) => VarRenameable (PolarShimWit shim w polarity t) where
    varRename ev = endoShimWit $ varRename ev

instance VarRenameable (TypeVarT t) where
    varRename ev = ev

instance VarRenameable (a :~: b) where
    varRename _ = mempty

-- hack to work around odd QualfifiedConstraints restriction
class (forall t. VarRenameable (w t)) => VarRenameable1 (w :: Type -> Type)

instance (forall t. VarRenameable (w t)) => VarRenameable1 (w :: Type -> Type)
