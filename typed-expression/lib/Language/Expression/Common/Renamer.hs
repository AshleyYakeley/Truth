module Language.Expression.Common.Renamer
    ( Renamer(..)
    , renameNegShimWit
    , renamePosShimWit
    , rename
    , NewVar(..)
    , VarNamespaceT
    , runVarNamespaceT
    , varNamespaceTRename
    , varNamespaceTAddNames
    , varNamespaceTLocal
    , VarRenamerT
    , runVarRenamerT
    , varRenamerTGenerate
    ) where

import Data.Shim
import Language.Expression.Common.WitnessMappable
import Shapes

data NewVar rn =
    forall t. MkNewVar (RenamerNegShimWit rn t)
                       (RenamerPosShimWit rn t)

class (MonadTransConstraint Monad rn, MonadTransConstraint Monad (RenamerNamespaceT rn), InCategory (RenamerShim rn)) =>
          Renamer rn where
    type RenamerNegWitness rn :: Type -> Type
    type RenamerPosWitness rn :: Type -> Type
    type RenamerNamespaceT rn :: (Type -> Type) -> (Type -> Type)
    type RenamerShim rn :: MapKind Type
    renameNegWitness :: Monad m => RenamerNegWitness rn t -> RenamerNamespaceT rn (rn m) (RenamerNegShimWit rn t)
    renamePosWitness :: Monad m => RenamerPosWitness rn t -> RenamerNamespaceT rn (rn m) (RenamerPosShimWit rn t)
    renameNewVar :: Monad m => rn m (NewVar rn)
    namespace :: Monad m => RenamerNamespaceT rn (rn m) r -> rn m r
    runRenamer :: Monad m => rn m r -> m r

type RenamerNegShimWit rn = ShimWit (RenamerShim rn) (RenamerNegWitness rn) 'Negative

type RenamerPosShimWit rn = ShimWit (RenamerShim rn) (RenamerPosWitness rn) 'Positive

renameNegShimWit ::
       forall rn m t. (Renamer rn, Monad m)
    => RenamerNegShimWit rn t
    -> RenamerNamespaceT rn (rn m) (RenamerNegShimWit rn t)
renameNegShimWit =
    case hasTransConstraint @Monad @rn @m of
        Dict ->
            case hasTransConstraint @Monad @(RenamerNamespaceT rn) @(rn m) of
                Dict -> chainShimWitM $ renameNegWitness @rn

renamePosShimWit ::
       forall rn m t. (Renamer rn, Monad m)
    => RenamerPosShimWit rn t
    -> RenamerNamespaceT rn (rn m) (RenamerPosShimWit rn t)
renamePosShimWit =
    case hasTransConstraint @Monad @rn @m of
        Dict ->
            case hasTransConstraint @Monad @(RenamerNamespaceT rn) @(rn m) of
                Dict -> chainShimWitM $ renamePosWitness @rn

rename ::
       forall rn m a. (Renamer rn, Monad m, WitnessMappable (RenamerPosShimWit rn) (RenamerNegShimWit rn) a)
    => a
    -> rn m a
rename a =
    withTransConstraintTM @Monad $
    namespace $ withTransConstraintTM @Monad $ mapWitnessesM renamePosShimWit renameNegShimWit a

newtype VarNamespaceT (ts :: Type) m a =
    MkVarNamespaceT (StateT [(String, String)] m a)
    deriving (Functor, Applicative, Monad, MonadFail, MonadTrans)

instance MonadTransConstraint Monad (VarNamespaceT ts) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail (VarNamespaceT ts) where
    hasTransConstraint = Dict

runVarNamespaceT :: Monad m => VarNamespaceT ts (VarRenamerT ts m) a -> VarRenamerT ts m a
runVarNamespaceT (MkVarNamespaceT ma) = do
    (a, _) <- runStateT ma []
    return a

varNamespaceTAddNames :: Monad m => [String] -> VarNamespaceT ts (VarRenamerT ts m) String
varNamespaceTAddNames oldnames = do
    pairs <- MkVarNamespaceT get
    newname <- lift $ varRenamerTGenerate oldnames
    MkVarNamespaceT $ put $ fmap (\oldname -> (oldname, newname)) oldnames <> pairs
    return newname

varNamespaceTRename :: Monad m => String -> VarNamespaceT ts (VarRenamerT ts m) String
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
varNamespaceTLocal ::
       Monad m => String -> (String -> VarNamespaceT ts (VarRenamerT ts m) a) -> VarNamespaceT ts (VarRenamerT ts m) a
varNamespaceTLocal oldname call = do
    newname <- lift $ varRenamerTGenerate [oldname]
    varNamespaceTAddMapping oldname newname
    a <- call newname
    varNamespaceTRemoveMapping oldname
    lift $ varRenamerTRemoveName newname
    return a

newtype VarRenamerT (ts :: Type) m a =
    MkVarRenamerT (StateT ([String], Int) m a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFail, MonadTrans)

instance MonadTransConstraint Monad (VarRenamerT ts) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail (VarRenamerT ts) where
    hasTransConstraint = Dict

varRenamerTRemoveName :: Monad m => String -> VarRenamerT ts m ()
varRenamerTRemoveName name =
    MkVarRenamerT $ do
        (oldvars, i) <- get
        let newvars = filter ((/=) name) oldvars
        put (newvars, i)

runVarRenamerT :: Monad m => VarRenamerT ts m a -> m a
runVarRenamerT (MkVarRenamerT ma) = do
    (a, _) <- runStateT ma ([], 0)
    return a

varName :: Int -> String
varName i
    | i < 26 = pure $ toEnum $ i + fromEnum 'a'
varName i = varName ((div i 26) - 1) <> (pure $ toEnum $ (mod i 26) + fromEnum 'a')

varRenamerTGenerate :: Monad m => [String] -> VarRenamerT ts m String
varRenamerTGenerate [] = do
    (vars, i) <- MkVarRenamerT $ get
    let newname = varName i
    if elem newname vars
        then do
            MkVarRenamerT $ put (vars, succ i)
            varRenamerTGenerate []
        else do
            MkVarRenamerT $ put (newname : vars, succ i)
            return newname
varRenamerTGenerate (name:nn) = do
    (vars, i) <- MkVarRenamerT $ get
    if elem name vars
        then varRenamerTGenerate nn
        else do
            MkVarRenamerT $ put (name : vars, succ i)
            return name
