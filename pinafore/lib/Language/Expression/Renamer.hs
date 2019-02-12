module Language.Expression.Renamer
    ( Renamer(..)
    , rename
    , NewVar(..)
    , VarNamespace
    , runVarNamespace
    , varNamespaceRename
    , VarRenamer
    , runVarRenamer
    , varRenamerGenerate
    , varRenamerGenerateSuggested
    ) where

import Language.Expression.Polarity
import Language.Expression.TypeF
import Language.Expression.TypeMappable
import Shapes

data NewVar rn =
    forall p q. MkNewVar (RenamerNegWitness rn q)
                         (RenamerPosWitness rn p)
                         (q -> p)

class (MonadTransConstraint Monad rn, MonadTransConstraint Monad (RenamerNamespace rn)) => Renamer rn where
    type RenamerNegWitness rn :: Type -> Type
    type RenamerPosWitness rn :: Type -> Type
    type RenamerNamespace rn :: (Type -> Type) -> (Type -> Type)
    renameTSNegWitness ::
           Monad m => RenamerNegWitness rn t -> RenamerNamespace rn (rn m) (TypeF (RenamerNegWitness rn) 'Negative t)
    renameTSPosWitness ::
           Monad m => RenamerPosWitness rn t -> RenamerNamespace rn (rn m) (TypeF (RenamerPosWitness rn) 'Positive t)
    renameNewVar :: Monad m => rn m (NewVar rn)
    namespace :: Monad m => RenamerNamespace rn (rn m) r -> rn m r
    runRenamer :: Monad m => rn m r -> m r

rename ::
       forall rn m a. (Renamer rn, Monad m, TypeMappable (->) (RenamerPosWitness rn) (RenamerNegWitness rn) a)
    => a
    -> rn m a
rename a =
    withTransConstraintTM @Monad $
    namespace $ withTransConstraintTM @Monad $ mapTypesM (renameTSPosWitness @rn) (renameTSNegWitness @rn) a

newtype VarNamespace (ts :: Type) m a =
    MkVarNamespace (StateT [(String, String)] m a)
    deriving (Functor, Applicative, Monad, MonadFail, MonadTrans)

instance MonadTransConstraint Monad (VarNamespace ts) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail (VarNamespace ts) where
    hasTransConstraint = Dict

runVarNamespace :: Monad m => VarNamespace ts (VarRenamer ts m) a -> VarRenamer ts m a
runVarNamespace (MkVarNamespace ma) = do
    (a, _) <- runStateT ma []
    return a

varNamespaceRename :: Monad m => String -> VarNamespace ts (VarRenamer ts m) String
varNamespaceRename oldname = do
    pairs <- MkVarNamespace get
    case lookup oldname pairs of
        Just newname -> return newname
        Nothing -> do
            newname <- MkVarNamespace $ lift $ varRenamerGenerateSuggested oldname
            MkVarNamespace $ put $ (oldname, newname) : pairs
            return newname

newtype VarRenamer (ts :: Type) m a =
    MkVarRenamer (StateT ([String], Int) m a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFail, MonadTrans)

instance MonadTransConstraint Monad (VarRenamer ts) where
    hasTransConstraint = Dict

instance MonadTransConstraint MonadFail (VarRenamer ts) where
    hasTransConstraint = Dict

runVarRenamer :: Monad m => VarRenamer ts m a -> m a
runVarRenamer (MkVarRenamer ma) = do
    (a, _) <- runStateT ma ([], 0)
    return a

varName :: Int -> String
varName i
    | i < 26 = pure $ toEnum $ i + fromEnum 'a'
varName i = varName ((div i 26) - 1) <> (pure $ toEnum $ (mod i 26) + fromEnum 'a')

varRenamerGenerate :: Monad m => VarRenamer ts m String
varRenamerGenerate = do
    (vars, i) <- MkVarRenamer get
    let newname = varName i
    if elem newname vars
        then do
            MkVarRenamer $ put (vars, succ i)
            varRenamerGenerate
        else do
            MkVarRenamer $ put (newname : vars, succ i)
            return newname

varRenamerGenerateSuggested :: Monad m => String -> VarRenamer ts m String
varRenamerGenerateSuggested name = do
    (vars, i) <- MkVarRenamer $ get
    if elem name vars
        then varRenamerGenerateSuggested (name <> "'")
        else do
            MkVarRenamer $ put (name : vars, succ i)
            return name
