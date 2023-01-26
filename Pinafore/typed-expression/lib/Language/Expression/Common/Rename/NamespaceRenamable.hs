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
        => EndoM (RenamerNamespaceT ts (RenamerT ts m)) t
    namespaceTypeNames :: t -> [String]

instance (RenameTypeSystem ts, forall t'. NamespaceRenamable ts (w t')) =>
             NamespaceRenamable ts (PolarShimWit shim w polarity t) where
    namespaceRename ::
           forall m. Monad m
        => EndoM (RenamerNamespaceT ts (RenamerT ts m)) (PolarShimWit shim w polarity t)
    namespaceRename =
        case hasTransConstraint @Monad @(RenamerT ts) @m of
            Dict ->
                case hasTransConstraint @Monad @(RenamerNamespaceT ts) @(RenamerT ts m) of
                    Dict -> endoShimWit $ namespaceRename @ts
    namespaceTypeNames (MkShimWit wt _) = namespaceTypeNames @ts wt

instance (RenamerNamespaceT ts ~ VarNamespaceT ts, RenamerT ts ~ VarRenamerT ts) => NamespaceRenamable ts (VarType t) where
    namespaceRename = MkEndoM $ \(MkVarType var) -> varNamespaceTRenameUVar @_ @_ @ts var
    namespaceTypeNames (MkVarType var) = [uVarName var]
