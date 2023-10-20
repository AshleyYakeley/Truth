module Language.Expression.Common.Rename.RenameTypeSystem
    ( RenameTypeSystem(..)
    , renameMappable
    , renameMappableSimple
    , renameType
    , renameTypeSignature
    , finalRenameMappable
    , NewVar(..)
    , namespaceRenameType
    ) where

import Data.Shim
import Language.Expression.Common.Rename.Rigidity
import Language.Expression.Common.Rename.VarRenameable
import Language.Expression.Common.TypeSystem
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
    namespaceRenameSource ::
           forall m. Monad m
        => RenameSource (RenamerNamespaceT ts (RenamerT ts m))
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
                Dict -> varRename $ namespaceRenameSource @ts

finalRenameMappable ::
       forall ts m a. (RenameTypeSystem ts, Monad m, TSMappable ts a)
    => EndoM (RenamerT ts m) a
finalRenameMappable = MkEndoM $ \a -> finalRenamer @ts $ renameMappableSimple @ts a

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

renameMappable ::
       forall ts m a. (RenameTypeSystem ts, Monad m, TSMappable ts a)
    => [String]
    -> NameRigidity
    -> a
    -> RenamerT ts m a
renameMappable fixedNames rigid a =
    withTransConstraintTM @Monad $
    namespace @ts fixedNames rigid $
    withTransConstraintTM @Monad $ unEndoM (mapWitnessesM (renamePosShimWit @ts) (renameNegShimWit @ts)) a

renameMappableSimple ::
       forall ts m a. (RenameTypeSystem ts, Monad m, TSMappable ts a)
    => a
    -> RenamerT ts m a
renameMappableSimple = renameMappable @ts [] FreeName

renameType ::
       forall ts m a. (RenameTypeSystem ts, Monad m, VarRenameable a)
    => [String]
    -> NameRigidity
    -> EndoM (RenamerT ts m) a
renameType fixedNames rigid =
    case hasTransConstraint @Monad @(RenamerT ts) @m of
        Dict -> hoistEndoM (namespace @ts fixedNames rigid) $ namespaceRenameType @ts

renameTypeSignature ::
       forall ts m. (RenameTypeSystem ts, Monad m)
    => EndoM (RenamerT ts m) (Some (TSPosWitness ts))
renameTypeSignature =
    case hasTransConstraint @Monad @(RenamerT ts) @m of
        Dict -> endoSomeFor $ hoistEndoM (namespace @ts [] RigidName) $ namespaceRenameType @ts
