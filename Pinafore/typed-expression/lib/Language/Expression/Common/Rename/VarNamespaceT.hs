module Language.Expression.Common.Rename.VarNamespaceT
    ( NameRigidity(..)
    , RenamerMonad(..)
    , renamerGenerateFree
    , renamerGenerateFreeUVar
    , VarNamespaceT
    , runVarNamespaceT
    , varNamespaceTRename
    , varNamespaceTRenameUVar
    ) where

import Language.Expression.Common.Rename.Rigidity
import Language.Expression.Common.TypeVariable
import Shapes

class Monad m => RenamerMonad m where
    renamerGenerate :: NameRigidity -> m String
    renamerRemoveName :: String -> m ()
    renamerGetNameRigidity :: m (String -> NameRigidity)

renamerGenerateFree ::
       forall m. RenamerMonad m
    => m String
renamerGenerateFree = renamerGenerate FreeName

renamerGenerateFreeUVar ::
       forall m. RenamerMonad m
    => m SomeTypeVarT
renamerGenerateFreeUVar = do
    newname <- renamerGenerateFree
    return $ newTypeVar newname MkSomeTypeVarT

data VNContext = MkVNContext
    { vncFixedNames :: [String]
    , vncRigidity :: NameRigidity
    }

newtype VarNamespaceT (ts :: Type) m a =
    MkVarNamespaceT (ReaderT VNContext (StateT [(String, String)] m) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

instance MonadTrans (VarNamespaceT ts) where
    lift ma = MkVarNamespaceT $ lift $ lift ma

instance TransConstraint Monad (VarNamespaceT ts) where
    hasTransConstraint = Dict

instance TransConstraint MonadIO (VarNamespaceT ts) where
    hasTransConstraint = Dict

instance TransConstraint MonadFail (VarNamespaceT ts) where
    hasTransConstraint = Dict

runVarNamespaceT :: RenamerMonad m => [String] -> NameRigidity -> VarNamespaceT ts m a -> m a
runVarNamespaceT fixedNames rigid (MkVarNamespaceT ma) = evalStateT (runReaderT ma (MkVNContext fixedNames rigid)) []

varNamespaceTAddName :: RenamerMonad m => String -> VarNamespaceT ts m String
varNamespaceTAddName oldname = do
    rigid <- MkVarNamespaceT $ asks vncRigidity
    pairs <- MkVarNamespaceT $ lift get
    newname <- lift $ renamerGenerate rigid
    MkVarNamespaceT $ lift $ put $ (oldname, newname) : pairs
    return newname

varNamespaceTRename :: RenamerMonad m => String -> VarNamespaceT ts m String
varNamespaceTRename oldname = do
    pairs <- MkVarNamespaceT $ lift get
    fixedNames <- MkVarNamespaceT $ asks vncFixedNames
    if elem oldname fixedNames
        then return oldname
        else case lookup oldname pairs of
                 Just newname -> return newname
                 Nothing -> varNamespaceTAddName oldname

varNamespaceTRenameUVar ::
       forall k name ts m. RenamerMonad m
    => SymbolType name
    -> VarNamespaceT ts m (TypeVar (UVar k name))
varNamespaceTRenameUVar oldvar = do
    newname <- varNamespaceTRename $ uVarName oldvar
    return $ newAssignTypeVar newname
