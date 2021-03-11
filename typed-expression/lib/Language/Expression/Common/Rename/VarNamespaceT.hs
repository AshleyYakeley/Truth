module Language.Expression.Common.Rename.VarNamespaceT
    ( RenamerMonad(..)
    , renamerGenerateUVar
    , VarNamespaceT
    , runVarNamespaceT
    , varNamespaceTLocal
    , varNamespaceTRenameUVar
    , varNamespaceTLocalUVar
    ) where

import Language.Expression.Common.TypeVariable
import Shapes

class Monad m => RenamerMonad m where
    renamerGenerate :: [String] -> m String
    renamerRemoveName :: String -> m ()
    renamerGetRigidNames :: m [String]
    renamerRigid :: forall a. m a -> m a

renamerGenerateUVar ::
       forall k name m. RenamerMonad m
    => [SymbolType name]
    -> m (VarType (UVar k name))
renamerGenerateUVar oldvars = do
    newname <- renamerGenerate $ fmap uVarName oldvars
    return $ newAssignUVar newname

newtype VarNamespaceT (ts :: Type) m a =
    MkVarNamespaceT (StateT [(String, String)] m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadTrans)

instance MonadTransConstraint Monad (VarNamespaceT ts) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO (VarNamespaceT ts) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail (VarNamespaceT ts) where
    hasTransConstraint = Dict

runVarNamespaceT :: RenamerMonad m => Bool -> VarNamespaceT ts m a -> m a
runVarNamespaceT rigid (MkVarNamespaceT ma) =
    (if rigid
         then renamerRigid
         else id) $ do
        (a, _) <- runStateT ma []
        return a

varNamespaceTAddNames :: RenamerMonad m => [String] -> VarNamespaceT ts m String
varNamespaceTAddNames oldnames = do
    pairs <- MkVarNamespaceT get
    newname <- lift $ renamerGenerate oldnames
    MkVarNamespaceT $ put $ fmap (\oldname -> (oldname, newname)) oldnames <> pairs
    return newname

varNamespaceTRename :: RenamerMonad m => String -> VarNamespaceT ts m String
varNamespaceTRename oldname = do
    pairs <- MkVarNamespaceT get
    case lookup oldname pairs of
        Just newname -> return newname
        Nothing -> varNamespaceTAddNames [oldname]

-- | add a new mapping, even if one already exists for the old name
varNamespaceTAddMapping :: Monad m => String -> String -> VarNamespaceT ts m ()
varNamespaceTAddMapping oldname newname = do
    pairs <- MkVarNamespaceT get
    MkVarNamespaceT $ put $ (oldname, newname) : pairs

-- | remove a mapping (just one) for an old name
varNamespaceTRemoveMapping :: Monad m => String -> VarNamespaceT ts m ()
varNamespaceTRemoveMapping oldname =
    MkVarNamespaceT $ do
        oldmaps <- get
        let newmaps = deleteFirstMatching (\(n, _) -> oldname == n) oldmaps
        put newmaps

-- | Use this for variable quantifiers (e.g. rec, forall)
varNamespaceTLocal :: RenamerMonad m => String -> (String -> VarNamespaceT ts m a) -> VarNamespaceT ts m a
varNamespaceTLocal oldname call = do
    newname <- lift $ renamerGenerate [oldname]
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
