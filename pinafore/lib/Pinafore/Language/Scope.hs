module Pinafore.Language.Scope
    ( NamedType(..)
    , Scoped
    , runScoped
    , liftScoped
    , SourcePos
    , SourceScoped
    , askSourcePos
    , remonadSourcePos
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
    , getOpenEntitySubtype
    , TypeCheckSubtype(..)
    ) where

import Data.Shim
import Language.Expression.Error
import Pinafore.Language.Error
import Pinafore.Language.Name
import Pinafore.Language.Subtype
import Pinafore.Language.Type.TypeID
import Pinafore.Language.Value
import Shapes
import Text.Parsec (SourcePos)

data NamedType ct
    = OpenEntityNamedType
    | ClosedEntityNamedType ct

type OpenEntityShim = LiftedCategory JMShim OpenEntity

data Scope expr patc ct = MkScope
    { scopeBindings :: StrictMap Name expr
    , scopePatternConstructors :: StrictMap Name patc
    , scopeTypes :: StrictMap Name (TypeID, NamedType ct)
    , scopeOpenEntitySubtypes :: [SubtypeEntry OpenEntityShim TypeIDType]
    }

newtype Scoped expr patc ct a =
    MkScoped (ReaderT (Scope expr patc ct) (StateT TypeID InterpretResult) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance MonadError [ErrorMessage] (Scoped expr patc ct) where
    throwError err = MkScoped $ throwError err

instance MonadError ErrorMessage (Scoped expr patc ct) where
    throwError err = throwError [err]

instance Semigroup a => Semigroup (Scoped expr patc ct a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Scoped expr patc ct a) where
    mappend = (<>)
    mempty = pure mempty

runScoped :: Scoped expr patc ct a -> InterpretResult a
runScoped (MkScoped qa) = evalStateT (runReaderT qa $ MkScope mempty mempty mempty mempty) zeroTypeID

liftScoped :: InterpretResult a -> Scoped expr patc ct a
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

remonadSourcePos ::
       (forall a. Scoped expr1 patc1 ct1 a -> Scoped expr2 patc2 ct2 a)
    -> SourceScoped expr1 patc1 ct1 b
    -> SourceScoped expr2 patc2 ct2 b
remonadSourcePos mm (MkSourceScoped mb) = MkSourceScoped $ remonad mm mb

mapSourcePos ::
       SourcePos
    -> (SourceScoped expr patc ct a -> SourceScoped expr patc ct b)
    -> Scoped expr patc ct a
    -> Scoped expr patc ct b
mapSourcePos spos f ca = runSourcePos spos $ f $ liftSourcePos ca

runSourceScoped :: SourcePos -> SourceScoped expr patc ct a -> InterpretResult a
runSourceScoped spos spa = runScoped $ runSourcePos spos spa

spScope :: SourceScoped expr patc ct (Scope expr patc ct)
spScope = MkSourceScoped $ lift pScope

instance MonadError ExpressionError (SourceScoped expr patc ct) where
    throwError err = throwError $ ExpressionErrorError err

instance MonadError [ErrorMessage] (SourceScoped expr patc ct) where
    throwError err = MkSourceScoped $ throwError err

instance MonadError ErrorMessage (SourceScoped expr patc ct) where
    throwError err = MkSourceScoped $ throwError err

instance MonadError ErrorType (SourceScoped expr patc ct) where
    throwError err =
        MkSourceScoped $ do
            spos <- ask
            throwError $ MkErrorMessage spos err

convertFailure :: Text -> Text -> SourceScoped expr patc ct a
convertFailure ta tb = throwError $ TypeConvertError ta tb

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
        Nothing -> throwError $ LookupTypeUnknownError name

lookupPatternConstructorM :: Name -> SourceScoped expr patc ct (Maybe patc)
lookupPatternConstructorM name = do
    (scopePatternConstructors -> names) <- spScope
    return $ lookup name names

lookupPatternConstructor :: Name -> SourceScoped expr patc ct patc
lookupPatternConstructor name = do
    ma <- lookupPatternConstructorM name
    case ma of
        Just a -> return a
        Nothing -> throwError $ LookupConstructorUnknownError name

withNewTypeName ::
       Name
    -> NamedType ct
    -> SourceScoped expr patc ct (TypeID, WMFunction (Scoped expr patc ct) (Scoped expr patc ct))
withNewTypeName name t = do
    mnt <- lookupNamedTypeM name
    case mnt of
        Just _ -> throwError $ DeclareTypeDuplicateError name
        Nothing -> do
            tid <-
                liftSourcePos $
                MkScoped $
                lift $ do
                    tid <- get
                    put $ succTypeID tid
                    return tid
            return $
                (tid, MkWMFunction $ pLocalScope (\tc -> tc {scopeTypes = insertMap name (tid, t) (scopeTypes tc)}))

withNewPatternConstructor ::
       Name -> patc -> SourceScoped expr patc ct (WMFunction (Scoped expr patc ct) (Scoped expr patc ct))
withNewPatternConstructor name pc = do
    ma <- lookupPatternConstructorM name
    case ma of
        Just _ -> throwError $ DeclareConstructorDuplicateError name
        Nothing ->
            return $
            MkWMFunction $
            pLocalScope (\tc -> tc {scopePatternConstructors = insertMap name pc $ scopePatternConstructors tc})

withNewPatternConstructors :: StrictMap Name patc -> Scoped expr patc ct a -> Scoped expr patc ct a
withNewPatternConstructors pp = pLocalScope (\tc -> tc {scopePatternConstructors = pp <> scopePatternConstructors tc})

withEntitySubtype ::
       TypeIDType tida
    -> TypeIDType tidb
    -> SourceScoped expr patc ct (WMFunction (Scoped expr patc ct) (Scoped expr patc ct))
withEntitySubtype ta tb =
    return $
    MkWMFunction $
    pLocalScope
        (\tc ->
             tc
                 { scopeOpenEntitySubtypes =
                       (MkSubtypeEntry ta tb $ MkLiftedCategory $ coerceEnhanced "open entity subtype") :
                       (scopeOpenEntitySubtypes tc)
                 })

getOpenEntitySubtype ::
       Name
    -> TypeIDType tida
    -> Name
    -> TypeIDType tidb
    -> SourceScoped expr patc ct (JMShim (OpenEntity tida) (OpenEntity tidb))
getOpenEntitySubtype na wa nb wb = do
    (scopeOpenEntitySubtypes -> subtypes) <- spScope
    case unSubtypeMatch (getSubtypeShim subtypes equalSubtypeMatch) wa wb of
        Just (MkLiftedCategory conv) -> return conv
        Nothing -> convertFailure (pack $ show na) (pack $ show nb)

class TypeCheckSubtype w where
    getSubtype :: forall expr patc ct a b. w a -> w b -> SourceScoped expr patc ct (a -> b)
