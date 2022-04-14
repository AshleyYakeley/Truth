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
    MkVarRenamerT (StateT RenamerState m a)
    deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, MonadFail, MonadTrans, MonadTransTunnel)

instance TransConstraint Functor (VarRenamerT ts) where
    hasTransConstraint = Dict

instance TransConstraint Monad (VarRenamerT ts) where
    hasTransConstraint = Dict

instance TransConstraint MonadIO (VarRenamerT ts) where
    hasTransConstraint = Dict

instance TransConstraint MonadFail (VarRenamerT ts) where
    hasTransConstraint = Dict

runVarRenamerT :: Monad m => VarRenamerT ts m a -> m a
runVarRenamerT (MkVarRenamerT sma) = let
    rsAssignedNames = []
    rsRigidNames = []
    rsIndex = 0
    in evalStateT sma MkRenamerState {..}

varName :: Int -> String
varName i
    | i < 26 = pure $ toEnum $ i + fromEnum 'a'
varName i = varName (pred (div i 26)) <> (pure $ toEnum $ (mod i 26) + fromEnum 'a')

modifyState :: Monad m => (RenamerState -> RenamerState) -> VarRenamerT ts m ()
modifyState ff = MkVarRenamerT $ modify ff

incIndex :: Monad m => VarRenamerT ts m ()
incIndex = modifyState $ \state -> state {rsIndex = succ $ rsIndex state}

assignName :: Monad m => NameRigidity -> String -> VarRenamerT ts m String
assignName rgd name = do
    modifyState $ \state -> state {rsAssignedNames = name : rsAssignedNames state}
    case rgd of
        FreeName -> return ()
        RigidName -> modifyState $ \state -> state {rsRigidNames = name : rsRigidNames state}
    return name

instance Monad m => RenamerMonad (VarRenamerT ts m) where
    renamerGenerate :: NameRigidity -> [String] -> VarRenamerT ts m String
    renamerGenerate rgd [] = do
        state <- MkVarRenamerT get
        let newname = varName $ rsIndex state
        incIndex
        if elem newname $ rsAssignedNames state
            then renamerGenerate rgd []
            else assignName rgd newname
    renamerGenerate rgd (name:nn) = do
        state <- MkVarRenamerT get
        let vars = rsAssignedNames state
        if elem name vars
            then renamerGenerate rgd nn
            else assignName rgd name
    renamerRemoveName :: String -> VarRenamerT ts m ()
    renamerRemoveName name =
        modifyState $ \state -> let
            newvars = filter ((/=) name) $ rsAssignedNames state
            in state {rsAssignedNames = newvars}
    renamerGetNameRigidity :: VarRenamerT ts m (String -> NameRigidity)
    renamerGetNameRigidity = do
        state <- MkVarRenamerT get
        return $ \name ->
            if elem name $ rsRigidNames state
                then RigidName
                else FreeName
