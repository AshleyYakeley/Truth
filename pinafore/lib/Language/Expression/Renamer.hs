module Language.Expression.Renamer
    ( Renamer(..)
    , rename
    , NewVar(..)
    , VarNamespaceT
    , runVarNamespaceT
    , varNamespaceTRename
    , VarRenamerT
    , runVarRenamerT
    , varRenamerTGenerate
    , varRenamerTGenerateSuggested
    ) where

import Language.Expression.Polarity
import Language.Expression.TypeF
import Language.Expression.TypeMappable
import Shapes

data NewVar rn =
    forall p q. MkNewVar (RenamerNegWitness rn q)
                         (RenamerPosWitness rn p)
                         (q -> p)

class (MonadTransConstraint Monad rn, MonadTransConstraint Monad (RenamerNamespaceT rn)) => Renamer rn where
    type RenamerNegWitness rn :: Type -> Type
    type RenamerPosWitness rn :: Type -> Type
    type RenamerNamespaceT rn :: (Type -> Type) -> (Type -> Type)
    renameTSNegWitness ::
           Monad m => RenamerNegWitness rn t -> RenamerNamespaceT rn (rn m) (TypeF (RenamerNegWitness rn) 'Negative t)
    renameTSPosWitness ::
           Monad m => RenamerPosWitness rn t -> RenamerNamespaceT rn (rn m) (TypeF (RenamerPosWitness rn) 'Positive t)
    renameNewVar :: Monad m => rn m (NewVar rn)
    namespace :: Monad m => RenamerNamespaceT rn (rn m) r -> rn m r
    runRenamer :: Monad m => rn m r -> m r

rename ::
       forall rn m a. (Renamer rn, Monad m, TypeMappable (->) (RenamerPosWitness rn) (RenamerNegWitness rn) a)
    => a
    -> rn m a
rename a =
    withTransConstraintTM @Monad $
    namespace $ withTransConstraintTM @Monad $ mapTypesM (renameTSPosWitness @rn) (renameTSNegWitness @rn) a

newtype VarNamespaceT (ts :: Type) m a =
    MkVarNamespaceT (StateT [(String, String)] m a)
    deriving (Functor, Applicative, Monad, MonadFail, MonadTrans)

instance MonadTransConstraint Monad (VarNamespaceT ts) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail (VarNamespaceT ts) where
    hasTransConstraint = Dict

runVarNamespaceT :: Monad m => VarNamespaceT ts (VarRenamerT ts m) a -> VarRenamerT ts m a
runVarNamespaceT (MkVarNamespaceT ma) = do
    (a, _) <- runStateT ma []
    return a

varNamespaceTRename :: Monad m => String -> VarNamespaceT ts (VarRenamerT ts m) String
varNamespaceTRename oldname = do
    pairs <- MkVarNamespaceT get
    case lookup oldname pairs of
        Just newname -> return newname
        Nothing -> do
            newname <- MkVarNamespaceT $ lift $ varRenamerTGenerateSuggested oldname
            MkVarNamespaceT $ put $ (oldname, newname) : pairs
            return newname

newtype VarRenamerT (ts :: Type) m a =
    MkVarRenamerT (StateT ([String], Int) m a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFail, MonadTrans)

instance MonadTransConstraint Monad (VarRenamerT ts) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail (VarRenamerT ts) where
    hasTransConstraint = Dict

runVarRenamerT :: Monad m => VarRenamerT ts m a -> m a
runVarRenamerT (MkVarRenamerT ma) = do
    (a, _) <- runStateT ma ([], 0)
    return a

varName :: Int -> String
varName i
    | i < 26 = pure $ toEnum $ i + fromEnum 'a'
varName i = varName ((div i 26) - 1) <> (pure $ toEnum $ (mod i 26) + fromEnum 'a')

varRenamerTGenerate :: Monad m => VarRenamerT ts m String
varRenamerTGenerate = do
    (vars, i) <- MkVarRenamerT get
    let newname = varName i
    if elem newname vars
        then do
            MkVarRenamerT $ put (vars, succ i)
            varRenamerTGenerate
        else do
            MkVarRenamerT $ put (newname : vars, succ i)
            return newname

varRenamerTGenerateSuggested :: Monad m => String -> VarRenamerT ts m String
varRenamerTGenerateSuggested name = do
    (vars, i) <- MkVarRenamerT $ get
    if elem name vars
        then varRenamerTGenerateSuggested (name <> "'")
        else do
            MkVarRenamerT $ put (name : vars, succ i)
            return name
