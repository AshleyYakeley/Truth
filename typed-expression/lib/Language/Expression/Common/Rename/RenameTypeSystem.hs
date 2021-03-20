module Language.Expression.Common.Rename.RenameTypeSystem
    ( RenameTypeSystem(..)
    , renameNegShimWit
    , renamePosShimWit
    , rename
    , renameTypeSignature
    , NewVar(..)
    ) where

import Data.Shim
import Language.Expression.Common.Rename.Rigidity
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.WitnessMappable
import Shapes

data NewVar ts =
    forall t. MkNewVar (TSNegShimWit ts t)
                       (TSPosShimWit ts t)

class (TypeSystem ts, MonadTransConstraint Monad (RenamerT ts), MonadTransConstraint Monad (RenamerNamespaceT ts)) =>
          RenameTypeSystem (ts :: Type) where
    type RenamerT ts :: (Type -> Type) -> (Type -> Type)
    type RenamerNamespaceT ts :: (Type -> Type) -> (Type -> Type)
    renameNegWitness :: Monad m => TSNegWitness ts t -> RenamerNamespaceT ts (RenamerT ts m) (TSNegWitness ts t)
    renamePosWitness :: Monad m => TSPosWitness ts t -> RenamerNamespaceT ts (RenamerT ts m) (TSPosWitness ts t)
    renameNewFreeVar :: Monad m => RenamerT ts m (NewVar ts)
    namespace :: Monad m => NameRigidity -> RenamerNamespaceT ts (RenamerT ts m) r -> RenamerT ts m r
    runRenamer :: Monad m => RenamerT ts m r -> m r

renameNegShimWit ::
       forall ts m t. (RenameTypeSystem ts, Monad m)
    => TSNegShimWit ts t
    -> RenamerNamespaceT ts (RenamerT ts m) (TSNegShimWit ts t)
renameNegShimWit =
    case hasTransConstraint @Monad @(RenamerT ts) @m of
        Dict ->
            case hasTransConstraint @Monad @(RenamerNamespaceT ts) @(RenamerT ts m) of
                Dict ->
                    \(MkShimWit t conv) -> do
                        t' <- renameNegWitness @ts t
                        return $ MkShimWit t' conv

renamePosShimWit ::
       forall ts m t. (RenameTypeSystem ts, Monad m)
    => TSPosShimWit ts t
    -> RenamerNamespaceT ts (RenamerT ts m) (TSPosShimWit ts t)
renamePosShimWit =
    case hasTransConstraint @Monad @(RenamerT ts) @m of
        Dict ->
            case hasTransConstraint @Monad @(RenamerNamespaceT ts) @(RenamerT ts m) of
                Dict ->
                    \(MkShimWit t conv) -> do
                        t' <- renamePosWitness @ts t
                        return $ MkShimWit t' conv

rename ::
       forall ts m a. (RenameTypeSystem ts, Monad m, WitnessMappable (TSPosShimWit ts) (TSNegShimWit ts) a)
    => a
    -> RenamerT ts m a
rename a =
    withTransConstraintTM @Monad $
    namespace @ts FreeName $
    withTransConstraintTM @Monad $ mapWitnessesM (renamePosShimWit @ts) (renameNegShimWit @ts) a

renameTypeSignature ::
       forall ts m. (RenameTypeSystem ts, Monad m)
    => AnyW (TSPosWitness ts)
    -> RenamerT ts m (AnyW (TSPosWitness ts))
renameTypeSignature (MkAnyW t) =
    withTransConstraintTM @Monad $ do
        t' <- namespace @ts RigidName $ renamePosWitness @ts t
        return $ MkAnyW t'
