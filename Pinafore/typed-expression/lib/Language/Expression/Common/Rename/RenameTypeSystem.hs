module Language.Expression.Common.Rename.RenameTypeSystem
    ( RenameTypeSystem(..)
    , rename
    , renameSimple
    , renameTypeSignature
    , finalRename
    , NewVar(..)
    , namespaceRenameType
    , typeSignatureNames
    ) where

import Data.Shim
import Language.Expression.Common.Rename.Rigidity
import Language.Expression.Common.Rename.VarRenameable
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.TypeVariable
import Language.Expression.Common.WitnessMappable
import Shapes

data NewVar ts =
    forall t. MkNewVar (TSNegShimWit ts t)
                       (TSPosShimWit ts t)

class ( TypeSystem ts
      , TransConstraint Monad (RenamerT ts)
      , MonadTrans (RenamerT ts)
      , VarRenameable1 (TSPosWitness ts)
      , VarRenameable1 (TSNegWitness ts)
      , TransConstraint Monad (RenamerNamespaceT ts)
      ) => RenameTypeSystem (ts :: Type) where
    type RenamerT ts :: (Type -> Type) -> (Type -> Type)
    type RenamerNamespaceT ts :: (Type -> Type) -> (Type -> Type)
    namespaceRenameTypeVar ::
           forall m. Monad m
        => EndoM' (RenamerNamespaceT ts (RenamerT ts m)) TypeVarT
    renameNewFreeVar ::
           forall m. Monad m
        => RenamerT ts m (NewVar ts)
    namespace ::
           forall m. Monad m
        => [String]
        -> NameRigidity
        -> RenamerNamespaceT ts (RenamerT ts m) --> RenamerT ts m
    runRenamer ::
           forall m. Monad m
        => [String]
        -> [String]
        -> RenamerT ts m --> m
    finalRenamer ::
           forall m. Monad m
        => RenamerT ts m --> RenamerT ts m

namespaceRenameType ::
       forall ts m a. (RenameTypeSystem ts, Monad m, VarRenameable a)
    => EndoM (RenamerNamespaceT ts (RenamerT ts m)) a
namespaceRenameType =
    case hasTransConstraint @Monad @(RenamerT ts) @m of
        Dict ->
            case hasTransConstraint @Monad @(RenamerNamespaceT ts) @(RenamerT ts m) of
                Dict -> varRename $ namespaceRenameTypeVar @ts

finalRename ::
       forall ts m a. (RenameTypeSystem ts, Monad m, TSMappable ts a)
    => EndoM (RenamerT ts m) a
finalRename = MkEndoM $ \a -> finalRenamer @ts $ renameSimple @ts a

renameNegShimWit ::
       forall ts m. (RenameTypeSystem ts, Monad m)
    => EndoM' (RenamerNamespaceT ts (RenamerT ts m)) (TSNegShimWit ts)
renameNegShimWit =
    case hasTransConstraint @Monad @(RenamerT ts) @m of
        Dict ->
            case hasTransConstraint @Monad @(RenamerNamespaceT ts) @(RenamerT ts m) of
                Dict -> endoShimWit $ namespaceRenameType @ts

renamePosShimWit ::
       forall ts m. (RenameTypeSystem ts, Monad m)
    => EndoM' (RenamerNamespaceT ts (RenamerT ts m)) (TSPosShimWit ts)
renamePosShimWit =
    case hasTransConstraint @Monad @(RenamerT ts) @m of
        Dict ->
            case hasTransConstraint @Monad @(RenamerNamespaceT ts) @(RenamerT ts m) of
                Dict -> endoShimWit $ namespaceRenameType @ts

rename ::
       forall ts m a. (RenameTypeSystem ts, Monad m, TSMappable ts a)
    => [String]
    -> NameRigidity
    -> a
    -> RenamerT ts m a
rename fixedNames rigid a =
    withTransConstraintTM @Monad $
    namespace @ts fixedNames rigid $
    withTransConstraintTM @Monad $ unEndoM (mapWitnessesM (renamePosShimWit @ts) (renameNegShimWit @ts)) a

renameSimple ::
       forall ts m a. (RenameTypeSystem ts, Monad m, TSMappable ts a)
    => a
    -> RenamerT ts m a
renameSimple = rename @ts [] FreeName

typeSignatureNames ::
       forall ts. RenameTypeSystem ts
    => Some (TSPosWitness ts)
    -> [String]
typeSignatureNames (MkSome t) = renameableVars t

renameTypeSignature ::
       forall ts m. (RenameTypeSystem ts, Monad m)
    => EndoM (RenamerT ts m) (Some (TSPosWitness ts))
renameTypeSignature =
    case hasTransConstraint @Monad @(RenamerT ts) @m of
        Dict -> endoSomeFor $ hoistEndoM (namespace @ts [] RigidName) $ namespaceRenameType @ts
