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
import Pinafore.Language.OpenEntity
import Pinafore.Language.TypeID
import Shapes
import Text.Parsec (SourcePos)

data NamedType ct
    = OpenEntityNamedType
    | ClosedEntityNamedType ct

data Scope expr patc ct = MkScope
    { scopeBindings :: StrictMap Name expr
    , scopePatternConstructors :: StrictMap Name patc
    , scopeTypes :: StrictMap Name (TypeID, NamedType ct)
    , scopeEntitySubtypes :: [(TypeID, TypeID)]
    }

newtype Scoped expr patc ct a =
    MkScoped (ReaderT (Scope expr patc ct) (StateT TypeID (Result Text)) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFail)

instance Semigroup a => Semigroup (Scoped expr patc ct a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Scoped expr patc ct a) where
    mappend = (<>)
    mempty = pure mempty

runScoped :: Scoped expr patc ct a -> Result Text a
runScoped (MkScoped qa) = evalStateT (runReaderT qa $ MkScope mempty mempty mempty mempty) zeroTypeID

liftScoped :: Result Text a -> Scoped expr patc ct a
liftScoped ra = MkScoped $ lift $ lift ra

pScope :: Scoped expr patc ct (Scope expr patc ct)
pScope = MkScoped ask

pLocalScope :: (Scope expr patc ct -> Scope expr patc ct) -> Scoped expr patc ct a -> Scoped expr patc ct a
pLocalScope maptc (MkScoped ma) = MkScoped $ local maptc ma

newtype SourceScoped expr patc ct a =
    MkSourceScoped (ReaderT SourcePos (Scoped expr patc ct) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

askSourcePos :: SourceScoped expr patc ct SourcePos
askSourcePos = MkSourceScoped ask

runSourcePos :: SourcePos -> SourceScoped expr patc ct a -> Scoped expr patc ct a
runSourcePos spos (MkSourceScoped ma) = runReaderT ma spos

liftSourcePos :: Scoped expr patc ct a -> SourceScoped expr patc ct a
liftSourcePos ma = MkSourceScoped $ lift ma

mapSourcePos ::
       SourcePos
    -> (SourceScoped expr patc ct a -> SourceScoped expr patc ct b)
    -> Scoped expr patc ct a
    -> Scoped expr patc ct b
mapSourcePos spos f ca = runSourcePos spos $ f $ liftSourcePos ca

runSourceScoped :: SourcePos -> SourceScoped expr patc ct a -> Result Text a
runSourceScoped spos spa = runScoped $ runSourcePos spos spa

spScope :: SourceScoped expr patc ct (Scope expr patc ct)
spScope = MkSourceScoped $ lift pScope

instance MonadFail (SourceScoped expr patc ct) where
    fail s =
        MkSourceScoped $ do
            spos <- ask
            lift $ fail $ show spos <> ": " <> s

convertFailure :: String -> String -> SourceScoped expr patc ct a
convertFailure sa sb = fail $ "cannot convert " <> show sa <> " to " <> show sb

lookupBinding :: Name -> SourceScoped expr patc ct (Maybe expr)
lookupBinding name = do
    (scopeBindings -> names) <- spScope
    return $ lookup name names

withNewBindings :: StrictMap Name expr -> Scoped expr patc ct a -> Scoped expr patc ct a
withNewBindings bb ma = pLocalScope (\tc -> tc {scopeBindings = bb <> (scopeBindings tc)}) ma

lookupNamedTypeM :: Name -> SourceScoped expr patc ct (Maybe (TypeID, NamedType ct))
lookupNamedTypeM name = do
    (scopeTypes -> names) <- spScope
    return $ lookup name names

lookupNamedType :: Name -> SourceScoped expr patc ct (TypeID, NamedType ct)
lookupNamedType name = do
    mnt <- lookupNamedTypeM name
    case mnt of
        Just nt -> return nt
        Nothing -> fail $ "unknown type: " <> unpack name

lookupPatternConstructorM :: Name -> SourceScoped expr patc ct (Maybe patc)
lookupPatternConstructorM name = do
    (scopePatternConstructors -> names) <- spScope
    return $ lookup name names

lookupPatternConstructor :: Name -> SourceScoped expr patc ct patc
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

castNamedEntity :: OpenEntity na -> OpenEntity nb
castNamedEntity (MkOpenEntity p) = MkOpenEntity p

withNewTypeName ::
       Name -> NamedType ct -> SourceScoped expr patc ct (TypeID, Transform (Scoped expr patc ct) (Scoped expr patc ct))
withNewTypeName name t = do
    mnt <- lookupNamedTypeM name
    case mnt of
        Just _ -> fail $ "duplicate type declaration: " <> unpack name
        Nothing -> do
            tid <-
                liftSourcePos $
                MkScoped $
                lift $ do
                    tid <- get
                    put $ succTypeID tid
                    return tid
            return $ (tid, MkTransform $ pLocalScope (\tc -> tc {scopeTypes = insertMap name (tid, t) (scopeTypes tc)}))

withNewPatternConstructor ::
       Name -> patc -> SourceScoped expr patc ct (Transform (Scoped expr patc ct) (Scoped expr patc ct))
withNewPatternConstructor name pc = do
    ma <- lookupPatternConstructorM name
    case ma of
        Just _ -> fail $ "duplicate constructor: " <> unpack name
        Nothing ->
            return $
            MkTransform $
            pLocalScope (\tc -> tc {scopePatternConstructors = insertMap name pc $ scopePatternConstructors tc})

withNewPatternConstructors :: StrictMap Name patc -> Scoped expr patc ct a -> Scoped expr patc ct a
withNewPatternConstructors pp = pLocalScope (\tc -> tc {scopePatternConstructors = pp <> scopePatternConstructors tc})

lookupOpenType :: Name -> SourceScoped expr patc ct TypeID
lookupOpenType n = do
    (tid, nt) <- lookupNamedType n
    case nt of
        OpenEntityNamedType -> return tid
        _ -> fail $ show n <> " is not an open entity type"

withEntitySubtype :: (Name, Name) -> SourceScoped expr patc ct (Transform (Scoped expr patc ct) (Scoped expr patc ct))
withEntitySubtype (a, b) = do
    ta <- lookupOpenType a
    tb <- lookupOpenType b
    return $ MkTransform $ pLocalScope (\tc -> tc {scopeEntitySubtypes = (ta, tb) : (scopeEntitySubtypes tc)})

getEntitySubtype ::
       Name
    -> TypeIDType tida
    -> Name
    -> TypeIDType tidb
    -> SourceScoped expr patc ct (OpenEntity tida -> OpenEntity tidb)
getEntitySubtype na wa nb wb = do
    (scopeEntitySubtypes -> subtypes) <- spScope
    if isSupertype subtypes [witnessToValue wa] (witnessToValue wb)
        then return castNamedEntity
        else convertFailure (show na) (show nb)

class TypeCheckSubtype w where
    getSubtype :: forall expr patc ct a b. w a -> w b -> SourceScoped expr patc ct (a -> b)
