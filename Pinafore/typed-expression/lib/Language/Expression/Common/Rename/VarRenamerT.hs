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
    deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadIO
             , MonadPlus
             , MonadFail
             , MonadTrans
             , MonadTransHoist
             , MonadTransTunnel
             )

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

debugForceNew :: Bool
debugForceNew = False

debugTagVar :: String -> String
debugTagVar n = n

renamerGenerateNew :: Monad m => NameRigidity -> VarRenamerT ts m String
renamerGenerateNew rgd = do
    state <- MkVarRenamerT get
    let newname = debugTagVar $ varName $ rsIndex state
    incIndex
    if elem newname $ rsAssignedNames state
        then renamerGenerateNew rgd
        else assignName rgd newname

instance Monad m => RenamerMonad (VarRenamerT ts m) where
    renamerGenerate :: NameRigidity -> Maybe String -> VarRenamerT ts m String
    renamerGenerate rgd _
        | debugForceNew = renamerGenerateNew rgd
    renamerGenerate rgd Nothing = renamerGenerateNew rgd
    renamerGenerate rgd (Just name) = do
        state <- MkVarRenamerT get
        if elem name $ rsAssignedNames state
            then renamerGenerateNew rgd
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
