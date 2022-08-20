module Language.Expression.Common.Rename.VarNamespaceT
    ( NameRigidity(..)
    , RenamerMonad(..)
    , renamerGenerateFree
    , renamerGenerateFreeUVar
    , VarNamespaceT
    , runVarNamespaceT
    , varNamespaceTRename
    , varNamespaceTLocal
    , varNamespaceTRenameUVar
    , varNamespaceTLocalUVar
    ) where

import Language.Expression.Common.Rename.Rigidity
import Language.Expression.Common.TypeVariable
import Shapes

class Monad m => RenamerMonad m where
    renamerGenerate :: NameRigidity -> Maybe String -> m String
    renamerRemoveName :: String -> m ()
    renamerGetNameRigidity :: m (String -> NameRigidity)

renamerGenerateFree ::
       forall m. RenamerMonad m
    => m String
renamerGenerateFree = renamerGenerate FreeName Nothing

renamerGenerateFreeUVar ::
       forall m. RenamerMonad m
    => m AnyVar
renamerGenerateFreeUVar = do
    newname <- renamerGenerateFree
    return $ newUVarAny newname

newtype VarNamespaceT (ts :: Type) m a =
    MkVarNamespaceT (ReaderT NameRigidity (StateT [(String, String)] m) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

instance MonadTrans (VarNamespaceT ts) where
    lift ma = MkVarNamespaceT $ lift $ lift ma

instance TransConstraint Monad (VarNamespaceT ts) where
    hasTransConstraint = Dict

instance TransConstraint MonadIO (VarNamespaceT ts) where
    hasTransConstraint = Dict

instance TransConstraint MonadFail (VarNamespaceT ts) where
    hasTransConstraint = Dict

runVarNamespaceT :: RenamerMonad m => NameRigidity -> VarNamespaceT ts m a -> m a
runVarNamespaceT rigid (MkVarNamespaceT ma) = evalStateT (runReaderT ma rigid) []

varNamespaceTAddName :: RenamerMonad m => String -> VarNamespaceT ts m String
varNamespaceTAddName oldname = do
    rigid <- MkVarNamespaceT ask
    pairs <- MkVarNamespaceT $ lift get
    newname <- lift $ renamerGenerate rigid $ Just oldname
    MkVarNamespaceT $ lift $ put $ (oldname, newname) : pairs
    return newname

varNamespaceTRename :: RenamerMonad m => String -> VarNamespaceT ts m String
varNamespaceTRename oldname = do
    pairs <- MkVarNamespaceT $ lift get
    case lookup oldname pairs of
        Just newname -> return newname
        Nothing -> varNamespaceTAddName oldname

-- | add a new mapping, even if one already exists for the old name
varNamespaceTAddMapping :: Monad m => String -> String -> VarNamespaceT ts m ()
varNamespaceTAddMapping oldname newname = do
    pairs <- MkVarNamespaceT $ lift get
    MkVarNamespaceT $ lift $ put $ (oldname, newname) : pairs

-- | remove a mapping (just one) for an old name
varNamespaceTRemoveMapping :: Monad m => String -> VarNamespaceT ts m ()
varNamespaceTRemoveMapping oldname =
    MkVarNamespaceT $
    lift $ do
        oldmaps <- get
        let newmaps = deleteFirstMatching (\(n, _) -> oldname == n) oldmaps
        put newmaps

-- | Use this for variable quantifiers (e.g. rec, forall)
varNamespaceTLocal :: RenamerMonad m => String -> (String -> VarNamespaceT ts m a) -> VarNamespaceT ts m a
varNamespaceTLocal oldname call = do
    newname <- lift $ renamerGenerate FreeName $ Just oldname
    varNamespaceTAddMapping oldname newname
    a <- call newname
    varNamespaceTRemoveMapping oldname
    lift $ renamerRemoveName newname
    return a

varNamespaceTRenameUVar ::
       forall k name ts m. RenamerMonad m
    => SymbolType name
    -> VarNamespaceT ts m (VarType (UVar k name))
varNamespaceTRenameUVar oldvar = do
    newname <- varNamespaceTRename $ uVarName oldvar
    return $ newAssignUVar newname

-- | Use this for variable quantifiers (e.g. rec, forall)
varNamespaceTLocalUVar ::
       forall k name ts m a. RenamerMonad m
    => SymbolType name
    -> (VarType (UVar k name) -> VarNamespaceT ts m a)
    -> VarNamespaceT ts m a
varNamespaceTLocalUVar oldvar call = varNamespaceTLocal (uVarName oldvar) $ \newname -> call $ newAssignUVar newname
