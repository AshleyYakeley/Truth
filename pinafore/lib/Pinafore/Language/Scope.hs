module Pinafore.Language.Scope
    ( NamedType(..)
    , Scoped
    , runScoped
    , liftScoped
    , SourcePos
    , SourceScoped
    , askSourcePos
    , runSourcePos
    , liftSourcePos
    , mapSourcePos
    , runSourceScoped
    , convertFailure
    , lookupBinding
    , withNewBindings
    , lookupNamedType
    , withNewTypeName
    , withEntitySubtype
    , getEntitySubtype
    , TypeCheckSubtype(..)
    ) where

import Pinafore.Language.Name
import Pinafore.Language.NamedEntity
import Shapes
import Text.Parsec (SourcePos)

data NamedType =
    EntityNamedType (AnyW SymbolType)

data Scope expr = MkScope
    { scopeBindings :: StrictMap Name expr
    , scopeTypes :: StrictMap Name NamedType
    , scopeEntitySubtypes :: [(Name, Name)]
    }

newtype Scoped expr a =
    MkScoped (ReaderT (Scope expr) (Result Text) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFail)

instance Semigroup a => Semigroup (Scoped expr a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Scoped expr a) where
    mappend = (<>)
    mempty = pure mempty

runScoped :: Scoped expr a -> Result Text a
runScoped (MkScoped qa) = runReaderT qa $ MkScope mempty mempty mempty

liftScoped :: Result Text a -> Scoped expr a
liftScoped ra = MkScoped $ lift ra

pScope :: Scoped expr (Scope expr)
pScope = MkScoped ask

pLocalScope :: (Scope expr -> Scope expr) -> Scoped expr a -> Scoped expr a
pLocalScope maptc (MkScoped ma) = MkScoped $ local maptc ma

newtype SourceScoped expr a =
    MkSourceScoped (ReaderT SourcePos (Scoped expr) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

askSourcePos :: SourceScoped expr SourcePos
askSourcePos = MkSourceScoped ask

runSourcePos :: SourcePos -> SourceScoped expr a -> Scoped expr a
runSourcePos spos (MkSourceScoped ma) = runReaderT ma spos

liftSourcePos :: Scoped expr a -> SourceScoped expr a
liftSourcePos ma = MkSourceScoped $ lift ma

mapSourcePos :: SourcePos -> (SourceScoped expr a -> SourceScoped expr b) -> Scoped expr a -> Scoped expr b
mapSourcePos spos f ca = runSourcePos spos $ f $ liftSourcePos ca

runSourceScoped :: SourcePos -> SourceScoped expr a -> Result Text a
runSourceScoped spos spa = runScoped $ runSourcePos spos spa

spScope :: SourceScoped expr (Scope expr)
spScope = MkSourceScoped $ lift pScope

instance MonadFail (SourceScoped expr) where
    fail s =
        MkSourceScoped $ do
            spos <- ask
            lift $ fail $ show spos <> ": " <> s

convertFailure :: String -> String -> SourceScoped expr a
convertFailure sa sb = fail $ "cannot convert " <> sa <> " to " <> sb

lookupBinding :: Name -> SourceScoped expr (Maybe expr)
lookupBinding name = do
    (scopeBindings -> names) <- spScope
    return $ lookup name names

withNewBindings :: StrictMap Name expr -> Scoped expr a -> Scoped expr a
withNewBindings bb ma = pLocalScope (\tc -> tc {scopeBindings = bb <> (scopeBindings tc)}) ma

lookupNamedTypeM :: Name -> SourceScoped expr (Maybe NamedType)
lookupNamedTypeM name = do
    (scopeTypes -> names) <- spScope
    return $ lookup name names

lookupNamedType :: Name -> SourceScoped expr NamedType
lookupNamedType name = do
    mnt <- lookupNamedTypeM name
    case mnt of
        Just nt -> return nt
        Nothing -> fail $ "unknown type: " <> unpack name

getImmediateSupertypes :: Eq a => [(a, b)] -> a -> [b]
getImmediateSupertypes st a0 = [(b) | (a, b) <- st, a == a0]

expandSupertypes :: Eq a => [(a, a)] -> [a] -> [a]
expandSupertypes st aa = nub $ mconcat $ aa : fmap (getImmediateSupertypes st) aa

isSupertype :: Eq a => [(a, a)] -> [a] -> a -> Bool
isSupertype _st aa a
    | elem a aa = True
isSupertype st aa a = let
    aa' = expandSupertypes st aa
    in if length aa' > length aa
           then isSupertype st aa' a
           else False

castNamedEntity :: NamedEntity na -> NamedEntity nb
castNamedEntity (MkNamedEntity p) = MkNamedEntity p

withNewTypeName :: Name -> NamedType -> SourceScoped expr (Transform (Scoped expr) (Scoped expr))
withNewTypeName name t = do
    mnt <- lookupNamedTypeM name
    case mnt of
        Just _ -> fail $ "duplicate type declaration: " <> unpack name
        Nothing -> return $ MkTransform $ pLocalScope (\tc -> tc {scopeTypes = insertMap name t (scopeTypes tc)})

withEntitySubtype :: (Name, Name) -> SourceScoped expr (Transform (Scoped expr) (Scoped expr))
withEntitySubtype rel@(a, b) = do
    _ <- lookupNamedType a
    _ <- lookupNamedType b
    return $ MkTransform $ pLocalScope (\tc -> tc {scopeEntitySubtypes = rel : (scopeEntitySubtypes tc)})

getEntitySubtype :: SymbolType na -> SymbolType nb -> SourceScoped expr (NamedEntity na -> NamedEntity nb)
getEntitySubtype wa wb = let
    sa = fromSymbolWitness wa
    sb = fromSymbolWitness wb
    in do
           (scopeEntitySubtypes -> subtypes) <- spScope
           if isSupertype subtypes [fromString sa] (fromString sb)
               then return castNamedEntity
               else convertFailure sa sb

class TypeCheckSubtype w where
    getSubtype :: forall expr a b. w a -> w b -> SourceScoped expr (a -> b)
