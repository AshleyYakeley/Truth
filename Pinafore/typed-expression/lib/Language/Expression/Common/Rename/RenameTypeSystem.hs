module Language.Expression.Common.Rename.RenameTypeSystem
    ( RenameTypeSystem(..)
    , rename
    , renameSimple
    , renameTypeSignature
    , finalRename
    , NewVar(..)
    , typeNamesWM
    , typeSignatureNames
    ) where

import Data.Shim
import Language.Expression.Common.Rename.Rigidity
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.WitnessMappable
import Shapes

data NewVar ts =
    forall t. MkNewVar (TSNegShimWit ts t)
                       (TSPosShimWit ts t)

class ( TypeSystem ts
      , TransConstraint Monad (RenamerT ts)
      , MonadTrans (RenamerT ts)
      , TransConstraint Monad (RenamerNamespaceT ts)
      ) => RenameTypeSystem (ts :: Type) where
    type RenamerT ts :: (Type -> Type) -> (Type -> Type)
    type RenamerNamespaceT ts :: (Type -> Type) -> (Type -> Type)
    renameNegWitness :: Monad m => EndoM (RenamerNamespaceT ts (RenamerT ts m)) (TSNegWitness ts t)
    renamePosWitness :: Monad m => EndoM (RenamerNamespaceT ts (RenamerT ts m)) (TSPosWitness ts t)
    typeNamesNegWitness :: TSNegWitness ts t -> [String]
    typeNamesPosWitness :: TSPosWitness ts t -> [String]
    renameNewFreeVar :: Monad m => RenamerT ts m (NewVar ts)
    namespace :: Monad m => [String] -> NameRigidity -> RenamerNamespaceT ts (RenamerT ts m) --> RenamerT ts m
    runRenamer :: Monad m => [String] -> [String] -> RenamerT ts m --> m
    finalRenamer :: Monad m => RenamerT ts m --> RenamerT ts m

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
                Dict -> endoShimWit $ renameNegWitness @ts

renamePosShimWit ::
       forall ts m. (RenameTypeSystem ts, Monad m)
    => EndoM' (RenamerNamespaceT ts (RenamerT ts m)) (TSPosShimWit ts)
renamePosShimWit =
    case hasTransConstraint @Monad @(RenamerT ts) @m of
        Dict ->
            case hasTransConstraint @Monad @(RenamerNamespaceT ts) @(RenamerT ts m) of
                Dict -> endoShimWit $ renamePosWitness @ts

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

typeNamesWM ::
       forall ts a. (RenameTypeSystem ts, TSMappable ts a)
    => a
    -> [String]
typeNamesWM a = do
    espsn <- mappableGetWitnesses @Type @(TSPosShimWit ts) @(TSNegShimWit ts) a
    case espsn of
        Left (MkSome (MkShimWit w _)) -> typeNamesPosWitness @ts w
        Right (MkSome (MkShimWit w _)) -> typeNamesNegWitness @ts w

typeSignatureNames ::
       forall ts. RenameTypeSystem ts
    => Some (TSPosWitness ts)
    -> [String]
typeSignatureNames (MkSome t) = typeNamesPosWitness @ts t

renameTypeSignature ::
       forall ts m. (RenameTypeSystem ts, Monad m)
    => EndoM (RenamerT ts m) (Some (TSPosWitness ts))
renameTypeSignature =
    case hasTransConstraint @Monad @(RenamerT ts) @m of
        Dict -> endoSomeFor $ hoistEndoM (namespace @ts [] RigidName) $ renamePosWitness @ts
