module Language.Expression.TypeSystem.Rename.VarRenamerT
    ( VarRenamerT
    , runVarRenamerT
    , finalVarRenamerT
    )
where

import Shapes

import Language.Expression.TypeSystem.Rename.VarNamespaceT

{-
The renamer renames variables in types.

Renaming type variables involves unsafe coercions.
Getting renaming wrong can break these coercions.
If you're seeing broken anchors, this is one place to check.

The renamer maps variables from source namespaces into a destination namespace.
All the variables in a type are in the same namespace.
Sometimes more than one type is in one namespace.
(Don't confuse type variable namespaces with declaration namespaces.)

The destination namespace may already have variables in it, known as fixed variables.
These are not renamed.
Variables in source namespaces are "renameable".

Each variable is either either rigid or free.
The renamer keeps track of which for the benefit of the unifier (renamerGetNameRigidity),
but otherwise doesn't care about the difference.

Each source namespace is either rigid or free, so all the variables from it are too.

The lists of fixed rigid variables and fixed free variables are passed to runRenamer (and don't change).

In the unifier, free variables (from inferred types) can be assigned to types and can be inverted,
while rigid variables (forall-quantified from type signatures) cannot be.
-}
data RenamerState = MkRenamerState
    { rsRigidNames :: [String]
    , rsIndex :: Int
    }

newtype VarRenamerT (ts :: Type) m a
    = MkVarRenamerT (ComposeT (ReaderT [String]) (StateT RenamerState) m a)
    deriving newtype
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadIO
        , MonadPlus
        , MonadFail
        , MonadTrans
        , MonadException
        , MonadTransHoist
        , MonadTransTunnel
        )

instance TransConstraint Monad (VarRenamerT ts) where
    hasTransConstraint = Dict

instance TransConstraint MonadIO (VarRenamerT ts) where
    hasTransConstraint = Dict

instance TransConstraint MonadFail (VarRenamerT ts) where
    hasTransConstraint = Dict

runVarRenamerT :: Monad m => [String] -> [String] -> VarRenamerT ts m --> m
runVarRenamerT rigidFixedNames freeFixedNames (MkVarRenamerT (MkComposeT rsma)) = let
    rsRigidNames = rigidFixedNames
    rsIndex = 0
    in evalStateT (runReaderT rsma $ rigidFixedNames <> freeFixedNames) $ MkRenamerState{..}

varName :: Int -> String
varName i
    | i < 26 = pure $ toEnum $ i + fromEnum 'a'
varName i = varName (pred (div i 26)) <> (pure $ toEnum $ (mod i 26) + fromEnum 'a')

debugTagVar :: String -> String
debugTagVar n = n

instance Monad m => RenamerMonad (VarRenamerT ts m) where
    renamerGenerate :: NameRigidity -> VarRenamerT ts m String
    renamerGenerate rgd = do
        state <- MkVarRenamerT $ MkComposeT $ lift get
        let
            i = rsIndex state
            name = debugTagVar $ varName i
        MkVarRenamerT $ MkComposeT $ lift $ put $ state{rsIndex = succ i}
        fixedNames <- MkVarRenamerT $ MkComposeT ask
        if elem name fixedNames
            then renamerGenerate @(VarRenamerT ts m) rgd
            else do
                case rgd of
                    FreeName -> return ()
                    RigidName -> MkVarRenamerT $ MkComposeT $ lift $ modify $ \st -> st{rsRigidNames = name : rsRigidNames st}
                return name
    renamerGetNameRigidity :: VarRenamerT ts m (String -> NameRigidity)
    renamerGetNameRigidity = do
        state <- MkVarRenamerT $ MkComposeT $ lift get
        return $ \name ->
            if elem name $ rsRigidNames state
                then RigidName
                else FreeName

finalVarRenamerT :: Monad m => VarRenamerT ts m --> VarRenamerT ts m
finalVarRenamerT (MkVarRenamerT (MkComposeT rsma)) =
    MkVarRenamerT
        $ MkComposeT
        $ hoist
            ( \sma -> do
                rs <- get
                lift $ evalStateT sma rs{rsIndex = 0}
            )
            rsma
