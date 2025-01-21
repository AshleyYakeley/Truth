module Language.Expression.TypeSystem.Rename.VarNamespaceT
    ( NameRigidity (..)
    , RenamerMonad (..)
    , renamerGenerateFree
    , renamerGenerateFreeTypeVarT
    , renamerGenerateAssign
    , fixedRenameSource
    , VarNamespaceT
    , runVarNamespaceT
    , varNamespaceTRename
    , varNamespaceRenameSource
    )
where

import Shapes

import Language.Expression.TypeSystem.Rename.RenameTypeSystem
import Language.Expression.TypeSystem.Rename.Rigidity
import Language.Expression.TypeSystem.Rename.VarRenameable
import Language.Expression.TypeSystem.TypeVariable

class Monad m => RenamerMonad m where
    renamerGenerate :: NameRigidity -> m String
    renamerGetNameRigidity :: m (String -> NameRigidity)

renamerGenerateFree ::
    forall m.
    RenamerMonad m =>
    m String
renamerGenerateFree = renamerGenerate FreeName

renamerGenerateFreeTypeVarT ::
    forall m.
    RenamerMonad m =>
    m SomeTypeVarT
renamerGenerateFreeTypeVarT = do
    newname <- renamerGenerateFree
    return $ newTypeVar newname MkSomeTypeVarT

renamerGenerateAssign ::
    forall m tv.
    RenamerMonad m =>
    m (TypeVarT tv)
renamerGenerateAssign = do
    newname <- renamerGenerateFree
    return $ newAssignTypeVar newname

fixedRenameSource :: RenamerMonad m => RenameSource m
fixedRenameSource = MkRenameSource{rsNewVar = MkEndoM $ \_ -> renamerGenerateAssign, rsRenameVar = mempty}

data VNContext = MkVNContext
    { vncFixedNames :: [String]
    , vncRigidity :: NameRigidity
    }

newtype VarNamespaceT (ts :: Type) m a
    = MkVarNamespaceT (ReaderT VNContext (StateT [(String, String)] m) a)
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail)

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

varNamespaceRenameSource ::
    forall ts m.
    RenamerMonad (RenamerT ts m) =>
    RenameSource (VarNamespaceT ts (RenamerT ts m))
varNamespaceRenameSource = let
    rsNewVar :: EndoM' (VarNamespaceT ts (RenamerT ts m)) TypeVarT
    rsNewVar =
        MkEndoM $ \(MkTypeVar _) -> do
            newname <- lift renamerGenerateFree
            return $ newAssignTypeVar newname
    rsRenameVar :: EndoM' (VarNamespaceT ts (RenamerT ts m)) TypeVarT
    rsRenameVar =
        MkEndoM $ \(MkTypeVar oldvar) -> do
            newname <- varNamespaceTRename $ uVarName oldvar
            return $ newAssignTypeVar newname
    in MkRenameSource{..}
