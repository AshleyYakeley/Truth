module Language.Expression.Common.Rename.RenameTypeSystem
    ( RenameTypeSystem(..)
    , rename
    , renameTypeSignature
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
    renameNegWitness :: Monad m => TSNegWitness ts t -> RenamerNamespaceT ts (RenamerT ts m) (TSNegWitness ts t)
    renamePosWitness :: Monad m => TSPosWitness ts t -> RenamerNamespaceT ts (RenamerT ts m) (TSPosWitness ts t)
    typeNamesNegWitness :: TSNegWitness ts t -> [String]
    typeNamesPosWitness :: TSPosWitness ts t -> [String]
    renameNewFreeVar :: Monad m => RenamerT ts m (NewVar ts)
    namespace :: Monad m => NameRigidity -> RenamerNamespaceT ts (RenamerT ts m) --> RenamerT ts m
    runRenamer :: Monad m => [String] -> RenamerT ts m --> m
    finalRenamer :: Monad m => RenamerT ts m --> RenamerT ts m

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
       forall ts m a. (RenameTypeSystem ts, Monad m, TSMappable ts a)
    => NameRigidity
    -> a
    -> RenamerT ts m a
rename rigid a =
    withTransConstraintTM @Monad $
    namespace @ts rigid $ withTransConstraintTM @Monad $ mapWitnessesM (renamePosShimWit @ts) (renameNegShimWit @ts) a

typeNamesWM ::
       forall ts a. (RenameTypeSystem ts, TSMappable ts a)
    => a
    -> [String]
typeNamesWM a = let
    tellPos :: forall t. TSPosShimWit ts t -> _ (TSPosShimWit ts t)
    tellPos w@(MkShimWit t _) = tell (typeNamesPosWitness @ts t) >> return w
    tellNeg :: forall t. TSNegShimWit ts t -> _ (TSNegShimWit ts t)
    tellNeg w@(MkShimWit t _) = tell (typeNamesNegWitness @ts t) >> return w
    in runIdentity $ execWriterT $ mapWitnessesM tellPos tellNeg a

typeSignatureNames ::
       forall ts. RenameTypeSystem ts
    => Some (TSPosWitness ts)
    -> [String]
typeSignatureNames (MkSome t) = typeNamesPosWitness @ts t

renameTypeSignature ::
       forall ts m. (RenameTypeSystem ts, Monad m)
    => Some (TSPosWitness ts)
    -> RenamerT ts m (Some (TSPosWitness ts))
renameTypeSignature (MkSome t) =
    withTransConstraintTM @Monad $ do
        t' <- namespace @ts RigidName $ renamePosWitness @ts t
        return $ MkSome t'
