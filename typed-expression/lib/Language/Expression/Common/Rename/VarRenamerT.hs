module Language.Expression.Common.Rename.VarRenamerT
    ( VarRenamerT
    , runVarRenamerT
    ) where

import Language.Expression.Common.Rename.VarNamespaceT
import Shapes

data RenamerState = MkRenamerState
    { rsAssignedNames :: [String]
    , rsRigidNames :: [String]
    , rsIndex :: Int
    }

newtype VarRenamerT (ts :: Type) m a =
    MkVarRenamerT (ReaderT Bool (StateT RenamerState m) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, MonadFail)

instance MonadTrans (VarRenamerT ts) where
    lift ma = MkVarRenamerT $ lift $ lift ma

instance MonadTransSemiTunnel (VarRenamerT ts) where
    semitunnel call =
        MkVarRenamerT $
        semitunnel $ \t1m1rm1a ->
            semitunnel $ \t2m1am1b -> call $ \(MkVarRenamerT t1t2m1r) -> t2m1am1b $ t1m1rm1a $ t1t2m1r

instance MonadTransConstraint Monad (VarRenamerT ts) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadIO (VarRenamerT ts) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail (VarRenamerT ts) where
    hasTransConstraint = Dict

runVarRenamerT :: Monad m => VarRenamerT ts m a -> m a
runVarRenamerT (MkVarRenamerT rma) = let
    rsAssignedNames = []
    rsRigidNames = []
    rsIndex = 0
    in evalStateT (runReaderT rma False) MkRenamerState {..}

varName :: Int -> String
varName i
    | i < 26 = pure $ toEnum $ i + fromEnum 'a'
varName i = varName (pred (div i 26)) <> (pure $ toEnum $ (mod i 26) + fromEnum 'a')

modifyState :: Monad m => (RenamerState -> RenamerState) -> VarRenamerT ts m ()
modifyState ff = MkVarRenamerT $ lift $ modify ff

incIndex :: Monad m => VarRenamerT ts m ()
incIndex = modifyState $ \state -> state {rsIndex = succ $ rsIndex state}

assignName :: Monad m => String -> VarRenamerT ts m ()
assignName name = modifyState $ \state -> state {rsAssignedNames = name : rsAssignedNames state}

rigidName :: Monad m => String -> VarRenamerT ts m ()
rigidName name = do
    rigid <- MkVarRenamerT ask
    if rigid
        then modifyState $ \state -> state {rsRigidNames = name : rsRigidNames state}
        else return ()

instance Monad m => RenamerMonad (VarRenamerT ts m) where
    renamerGenerate :: [String] -> VarRenamerT ts m String
    renamerGenerate [] = do
        state <- MkVarRenamerT $ lift get
        let newname = varName $ rsIndex state
        incIndex
        if elem newname $ rsAssignedNames state
            then renamerGenerate []
            else do
                assignName newname
                rigidName newname
                return newname
    renamerGenerate (name:nn) = do
        state <- MkVarRenamerT $ lift get
        let vars = rsAssignedNames state
        if elem name vars
            then renamerGenerate nn
            else do
                incIndex
                assignName name
                return name
    renamerRemoveName :: String -> VarRenamerT ts m ()
    renamerRemoveName name =
        modifyState $ \state -> let
            newvars = filter ((/=) name) $ rsAssignedNames state
            in state {rsAssignedNames = newvars}
    renamerGetIsNameRigid :: VarRenamerT ts m (String -> Bool)
    renamerGetIsNameRigid = do
        state <- MkVarRenamerT $ lift get
        return $ \name -> elem name $ rsRigidNames state
    renamerRigid :: Monad m => VarRenamerT ts m a -> VarRenamerT ts m a
    renamerRigid (MkVarRenamerT rma) = MkVarRenamerT $ local (\_ -> True) rma
