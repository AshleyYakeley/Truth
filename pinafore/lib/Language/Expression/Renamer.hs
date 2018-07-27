module Language.Expression.Renamer
    ( Namespace(..)
    , RenamerNegWitness
    , RenamerPosWitness
    , Renamer(..)
    , VarNamespace
    , runVarNamespace
    , varNamespaceRename
    , VarRenamer
    , runVarRenamer
    , varRenamerGenerate
    , varRenamerGenerateSuggested
    ) where

import Shapes

class Monad ns => Namespace ns where
    type NamespaceNegWitness ns :: Type -> Type
    type NamespacePosWitness ns :: Type -> Type
    renameNegWitness ::
           NamespaceNegWitness ns t -> (forall t'. NamespaceNegWitness ns t' -> Bijection t t' -> ns r) -> ns r
    renamePosWitness ::
           NamespacePosWitness ns t -> (forall t'. NamespacePosWitness ns t' -> Bijection t t' -> ns r) -> ns r

type RenamerNegWitness rn = NamespaceNegWitness (RenamerNamespace rn)

type RenamerPosWitness rn = NamespacePosWitness (RenamerNamespace rn)

class (Monad rn, Namespace (RenamerNamespace rn)) => Renamer rn where
    type RenamerNamespace rn :: Type -> Type
    renameNewVar :: (forall t. RenamerNegWitness rn t -> RenamerPosWitness rn t -> rn r) -> rn r
    namespace :: RenamerNamespace rn r -> rn r
    runRenamer :: rn r -> r

newtype VarNamespace ts a =
    MkVarNamespace (StateT [(String, String)] (VarRenamer ts) a)
    deriving (Functor, Applicative, Monad)

runVarNamespace :: VarNamespace ts a -> VarRenamer ts a
runVarNamespace (MkVarNamespace ma) = do
    (a, _) <- runStateT ma []
    return a

varNamespaceRename :: String -> VarNamespace ts String
varNamespaceRename oldname =
    MkVarNamespace $ do
        pairs <- get
        case lookup oldname pairs of
            Just newname -> return newname
            Nothing -> do
                newname <- lift $ varRenamerGenerateSuggested oldname
                put $ (oldname, newname) : pairs
                return newname

newtype VarRenamer ts a =
    MkVarRenamer (State ([String], Int) a)
    deriving (Functor, Applicative, Monad)

runVarRenamer :: VarRenamer ts a -> a
runVarRenamer (MkVarRenamer ga) = fst $ runState ga ([], 0)

varName :: Int -> String
varName i
    | i < 26 = pure $ toEnum $ i + fromEnum 'a'
varName i = varName ((div i 26) - 1) <> (pure $ toEnum $ (mod i 26) + fromEnum 'a')

varRenamerGenerate :: VarRenamer ts String
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

varRenamerGenerateSuggested :: String -> VarRenamer ts String
varRenamerGenerateSuggested name = do
    (vars, i) <- MkVarRenamer $ get
    if elem name vars
        then varRenamerGenerateSuggested (name <> "'")
        else do
            MkVarRenamer $ put (name : vars, succ i)
            return name
