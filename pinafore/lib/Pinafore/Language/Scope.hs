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
    , lookupPatternConstructor
    , withNewPatternConstructor
    , withNewPatternConstructors
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

data Scope expr patc = MkScope
    { scopeBindings :: StrictMap Name expr
    , scopePatternConstructors :: StrictMap Name patc
    , scopeTypes :: StrictMap Name NamedType
    , scopeEntitySubtypes :: [(Name, Name)]
    }

newtype Scoped expr patc a =
    MkScoped (ReaderT (Scope expr patc) (Result Text) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFail)

instance Semigroup a => Semigroup (Scoped expr patc a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Scoped expr patc a) where
    mappend = (<>)
    mempty = pure mempty

runScoped :: Scoped expr patc a -> Result Text a
runScoped (MkScoped qa) = runReaderT qa $ MkScope mempty mempty mempty mempty

liftScoped :: Result Text a -> Scoped expr patc a
liftScoped ra = MkScoped $ lift ra

pScope :: Scoped expr patc (Scope expr patc)
pScope = MkScoped ask

pLocalScope :: (Scope expr patc -> Scope expr patc) -> Scoped expr patc a -> Scoped expr patc a
pLocalScope maptc (MkScoped ma) = MkScoped $ local maptc ma

newtype SourceScoped expr patc a =
    MkSourceScoped (ReaderT SourcePos (Scoped expr patc) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

askSourcePos :: SourceScoped expr patc SourcePos
askSourcePos = MkSourceScoped ask

runSourcePos :: SourcePos -> SourceScoped expr patc a -> Scoped expr patc a
runSourcePos spos (MkSourceScoped ma) = runReaderT ma spos

liftSourcePos :: Scoped expr patc a -> SourceScoped expr patc a
liftSourcePos ma = MkSourceScoped $ lift ma

mapSourcePos ::
       SourcePos -> (SourceScoped expr patc a -> SourceScoped expr patc b) -> Scoped expr patc a -> Scoped expr patc b
mapSourcePos spos f ca = runSourcePos spos $ f $ liftSourcePos ca

runSourceScoped :: SourcePos -> SourceScoped expr patc a -> Result Text a
runSourceScoped spos spa = runScoped $ runSourcePos spos spa

spScope :: SourceScoped expr patc (Scope expr patc)
spScope = MkSourceScoped $ lift pScope

instance MonadFail (SourceScoped expr patc) where
    fail s =
        MkSourceScoped $ do
            spos <- ask
            lift $ fail $ show spos <> ": " <> s

convertFailure :: String -> String -> SourceScoped expr patc a
convertFailure sa sb = fail $ "cannot convert " <> sa <> " to " <> sb

lookupBinding :: Name -> SourceScoped expr patc (Maybe expr)
lookupBinding name = do
    (scopeBindings -> names) <- spScope
    return $ lookup name names

withNewBindings :: StrictMap Name expr -> Scoped expr patc a -> Scoped expr patc a
withNewBindings bb ma = pLocalScope (\tc -> tc {scopeBindings = bb <> (scopeBindings tc)}) ma

lookupNamedTypeM :: Name -> SourceScoped expr patc (Maybe NamedType)
lookupNamedTypeM name = do
    (scopeTypes -> names) <- spScope
    return $ lookup name names

lookupNamedType :: Name -> SourceScoped expr patc NamedType
lookupNamedType name = do
    mnt <- lookupNamedTypeM name
    case mnt of
        Just nt -> return nt
        Nothing -> fail $ "unknown type: " <> unpack name

lookupPatternConstructorM :: Name -> SourceScoped expr patc (Maybe patc)
lookupPatternConstructorM name = do
    (scopePatternConstructors -> names) <- spScope
    return $ lookup name names

lookupPatternConstructor :: Name -> SourceScoped expr patc patc
lookupPatternConstructor name = do
    ma <- lookupPatternConstructorM name
    case ma of
        Just a -> return a
        Nothing -> fail $ "unknown constructor: " <> unpack name

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

withNewTypeName :: Name -> NamedType -> SourceScoped expr patc (Transform (Scoped expr patc) (Scoped expr patc))
withNewTypeName name t = do
    mnt <- lookupNamedTypeM name
    case mnt of
        Just _ -> fail $ "duplicate type declaration: " <> unpack name
        Nothing -> return $ MkTransform $ pLocalScope (\tc -> tc {scopeTypes = insertMap name t (scopeTypes tc)})

withNewPatternConstructor :: Name -> patc -> SourceScoped expr patc (Transform (Scoped expr patc) (Scoped expr patc))
withNewPatternConstructor name pc = do
    ma <- lookupPatternConstructorM name
    case ma of
        Just _ -> fail $ "duplicate constructor: " <> unpack name
        Nothing ->
            return $
            MkTransform $
            pLocalScope (\tc -> tc {scopePatternConstructors = insertMap name pc $ scopePatternConstructors tc})

withNewPatternConstructors :: StrictMap Name patc -> Scoped expr patc a -> Scoped expr patc a
withNewPatternConstructors pp = pLocalScope (\tc -> tc {scopePatternConstructors = pp <> scopePatternConstructors tc})

withEntitySubtype :: (Name, Name) -> SourceScoped expr patc (Transform (Scoped expr patc) (Scoped expr patc))
withEntitySubtype rel@(a, b) = do
    _ <- lookupNamedType a
    _ <- lookupNamedType b
    return $ MkTransform $ pLocalScope (\tc -> tc {scopeEntitySubtypes = rel : (scopeEntitySubtypes tc)})

getEntitySubtype :: SymbolType na -> SymbolType nb -> SourceScoped expr patc (NamedEntity na -> NamedEntity nb)
getEntitySubtype wa wb = let
    sa = fromSymbolWitness wa
    sb = fromSymbolWitness wb
    in do
           (scopeEntitySubtypes -> subtypes) <- spScope
           if isSupertype subtypes [fromString sa] (fromString sb)
               then return castNamedEntity
               else convertFailure sa sb

class TypeCheckSubtype w where
    getSubtype :: forall expr patc a b. w a -> w b -> SourceScoped expr patc (a -> b)
