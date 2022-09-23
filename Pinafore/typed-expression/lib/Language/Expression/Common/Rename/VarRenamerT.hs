module Language.Expression.Common.Rename.VarRenamerT
    ( VarRenamerT
    , runVarRenamerT
    , finalVarRenamerT
    ) where

import Language.Expression.Common.Rename.VarNamespaceT
import Shapes

data RenamerState = MkRenamerState
    { rsRigidNames :: [String]
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
    rsRigidNames = []
    rsIndex = 0
    in evalStateT sma MkRenamerState {..}

varName :: Int -> String
varName i
    | i < 26 = pure $ toEnum $ i + fromEnum 'a'
varName i = varName (pred (div i 26)) <> (pure $ toEnum $ (mod i 26) + fromEnum 'a')

debugTagVar :: String -> String
debugTagVar n = n

renamerGenerateNew :: Monad m => NameRigidity -> VarRenamerT ts m String
renamerGenerateNew rgd = do
    state <- MkVarRenamerT get
    let
        i = rsIndex state
        newname = debugTagVar $ varName i
    MkVarRenamerT $ put $ state {rsIndex = succ i}
    renamerCheckGenerateNew rgd newname

renamerCheckGenerateNew :: Monad m => NameRigidity -> String -> VarRenamerT ts m String
renamerCheckGenerateNew rgd name = do
    state <- MkVarRenamerT get
    if elem name $ rsRigidNames state
        then renamerGenerateNew rgd
        else do
            case rgd of
                FreeName -> return ()
                RigidName -> MkVarRenamerT $ put $ state {rsRigidNames = name : rsRigidNames state}
            return name

instance Monad m => RenamerMonad (VarRenamerT ts m) where
    renamerGenerate :: NameRigidity -> Maybe String -> VarRenamerT ts m String
    renamerGenerate RigidName (Just name) = renamerCheckGenerateNew RigidName name
    renamerGenerate rgd _ = renamerGenerateNew rgd
    renamerRemoveName :: String -> VarRenamerT ts m ()
    renamerRemoveName _ = return ()
    renamerGetNameRigidity :: VarRenamerT ts m (String -> NameRigidity)
    renamerGetNameRigidity = do
        state <- MkVarRenamerT get
        return $ \name ->
            if elem name $ rsRigidNames state
                then RigidName
                else FreeName

finalVarRenamerT :: Monad m => VarRenamerT ts m --> VarRenamerT ts m
finalVarRenamerT (MkVarRenamerT sta) =
    MkVarRenamerT $ do
        state <- get
        lift $ evalStateT sta state {rsIndex = 0}
