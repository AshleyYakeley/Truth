module Language.Expression.Common.Rename.VarRenamerT
    ( VarRenamerT
    , runVarRenamerT
    , finalVarRenamerT
    ) where

import Language.Expression.Common.Rename.VarNamespaceT
import Shapes

{-
The renamer renames names in types.

By names, we mean type variables.
Renaming type variables involves unsafe coercions.
Getting renaming wrong can break these coercions.
If you're seeing broken anchors, this is one place to check.

All names come from namespaces.
All the names in a type are in the same namespace.
Sometimes more than one type is in one namespace.

Namespaces (and therefore names in them) are of three kinds:
* fixed rigid
* renameable rigid
* renameable free

Fixed names are not renamed, while renameable names are.
At most one namespace can be fixed (otherwise names in them would clash).
The list of fixed names is passed to runRenamer (and doesn't change).

The renamer keeps track of which names are free and which are rigid for the benefit of the unifier (renamerGetNameRigidity),
but otherwise doesn't care about the difference.
In the unifier, free names (from inferred types) can be assigned to types,
while rigid names (from type signatures) cannot be.
-}
data RenamerState = MkRenamerState
    { rsRigidNames :: [String]
    , rsIndex :: Int
    }

newtype VarRenamerT (ts :: Type) m a =
    MkVarRenamerT (ReaderT [String] (StateT RenamerState m) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, MonadException, MonadFail)

instance MonadTrans (VarRenamerT ts) where
    lift ma = MkVarRenamerT $ lift $ lift ma

instance MonadTransHoist (VarRenamerT ts) where
    hoist mm (MkVarRenamerT ma) = MkVarRenamerT $ hoist (hoist mm) ma

instance TransConstraint Monad (VarRenamerT ts) where
    hasTransConstraint = Dict

instance TransConstraint MonadIO (VarRenamerT ts) where
    hasTransConstraint = Dict

instance TransConstraint MonadFail (VarRenamerT ts) where
    hasTransConstraint = Dict

runVarRenamerT :: Monad m => [String] -> VarRenamerT ts m --> m
runVarRenamerT fixedNames (MkVarRenamerT rsma) = let
    rsRigidNames = fixedNames
    rsIndex = 0
    in evalStateT (runReaderT rsma fixedNames) $ MkRenamerState {..}

varName :: Int -> String
varName i
    | i < 26 = pure $ toEnum $ i + fromEnum 'a'
varName i = varName (pred (div i 26)) <> (pure $ toEnum $ (mod i 26) + fromEnum 'a')

debugTagVar :: String -> String
debugTagVar n = n

instance Monad m => RenamerMonad (VarRenamerT ts m) where
    renamerGenerate :: NameRigidity -> VarRenamerT ts m String
    renamerGenerate rgd = do
        state <- MkVarRenamerT $ lift get
        let
            i = rsIndex state
            name = debugTagVar $ varName i
        MkVarRenamerT $ lift $ put $ state {rsIndex = succ i}
        fixedNames <- MkVarRenamerT ask
        if elem name fixedNames
            then renamerGenerate @(VarRenamerT ts m) rgd
            else do
                case rgd of
                    FreeName -> return ()
                    RigidName -> MkVarRenamerT $ lift $ modify $ \st -> st {rsRigidNames = name : rsRigidNames st}
                return name
    renamerRemoveName :: String -> VarRenamerT ts m ()
    renamerRemoveName _ = return ()
    renamerGetNameRigidity :: VarRenamerT ts m (String -> NameRigidity)
    renamerGetNameRigidity = do
        state <- MkVarRenamerT $ lift get
        return $ \name ->
            if elem name $ rsRigidNames state
                then RigidName
                else FreeName

finalVarRenamerT :: Monad m => VarRenamerT ts m --> VarRenamerT ts m
finalVarRenamerT rma = do
    fixedNames <- MkVarRenamerT ask
    lift $ runVarRenamerT fixedNames rma
