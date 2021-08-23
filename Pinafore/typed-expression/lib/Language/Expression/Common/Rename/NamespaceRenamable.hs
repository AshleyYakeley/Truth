module Language.Expression.Common.Rename.NamespaceRenamable where

import Data.Shim
import Language.Expression.Common.Rename.RenameTypeSystem
import Language.Expression.Common.Rename.VarNamespaceT
import Language.Expression.Common.Rename.VarRenamerT
import Language.Expression.Common.TypeVariable
import Shapes

type NamespaceRenamable :: Type -> Type -> Constraint
class NamespaceRenamable ts t where
    namespaceRename ::
           forall m. Monad m
        => t
        -> RenamerNamespaceT ts (RenamerT ts m) t

instance (RenameTypeSystem ts, forall t'. NamespaceRenamable ts (w t')) =>
             NamespaceRenamable ts (PolarShimWit shim w polarity t) where
    namespaceRename (MkShimWit wt conv) =
        withTransConstraintTM' @Monad $
        withTransConstraintTM @Monad $ do
            wt' <- namespaceRename @ts wt
            return $ MkShimWit wt' conv

instance (RenamerNamespaceT ts ~ VarNamespaceT ts, RenamerT ts ~ VarRenamerT ts) => NamespaceRenamable ts (VarType t) where
    namespaceRename (MkVarType var) = varNamespaceTRenameUVar @_ @_ @ts var
