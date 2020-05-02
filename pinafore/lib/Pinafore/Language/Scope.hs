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
    , newTypeID
    , registerTypeName
    , lookupPatternConstructor
    , withNewPatternConstructor
    , withNewPatternConstructors
    , withEntitySubtype
    , getOpenEntitySubtype
    , TypeCheckSubtype(..)
    ) where

import Data.Shim
import Language.Expression.Dolan
import Language.Expression.Error
import Pinafore.Language.Error
import Pinafore.Language.Name
import Pinafore.Language.Subtype
import Pinafore.Language.Type.TypeID
import Pinafore.Language.TypeSystem.Show
import Pinafore.Language.Value
import Shapes
import Text.Parsec (SourcePos)

data NamedType (tw :: forall k. k -> Type) ct
    = forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). SimpleNamedType (DolanVarianceType dv)
                                                                                (DolanVarianceMap dv t)
                                                                                (ListTypeExprShow dv)
                                                                                (tw t)
    | OpenEntityNamedType TypeID
    | ClosedEntityNamedType TypeID
                            ct

type OpenEntityShim = LiftedCategory JMShim OpenEntity

data Scope expr patc (tw :: forall k. k -> Type) ct = MkScope
    { scopeBindings :: StrictMap Name expr
    , scopePatternConstructors :: StrictMap Name patc
    , scopeTypes :: StrictMap Name (NamedType tw ct)
    , scopeOpenEntitySubtypes :: [SubtypeEntry OpenEntityShim TypeIDType]
    }

newtype Scoped expr patc (tw :: forall k. k -> Type) ct a =
    MkScoped (ReaderT (Scope expr patc tw ct) (StateT TypeID InterpretResult) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance forall expr patc (tw :: forall k. k -> Type) ct. MonadThrow PinaforeError (Scoped expr patc tw ct) where
    throw err = MkScoped $ throw err

instance forall expr patc (tw :: forall k. k -> Type) ct. MonadThrow ErrorMessage (Scoped expr patc tw ct) where
    throw = throwErrorMessage

instance forall expr patc (tw :: forall k. k -> Type) ct a. Semigroup a => Semigroup (Scoped expr patc tw ct a) where
    (<>) = liftA2 (<>)

instance forall expr patc (tw :: forall k. k -> Type) ct a. Monoid a => Monoid (Scoped expr patc tw ct a) where
    mappend = (<>)
    mempty = pure mempty

runScoped :: forall expr patc (tw :: forall k. k -> Type) ct a. Scoped expr patc tw ct a -> InterpretResult a
runScoped (MkScoped qa) = evalStateT (runReaderT qa $ MkScope mempty mempty mempty mempty) zeroTypeID

liftScoped :: forall expr patc (tw :: forall k. k -> Type) ct a. InterpretResult a -> Scoped expr patc tw ct a
liftScoped ra = MkScoped $ lift $ lift ra

pScope :: forall expr patc (tw :: forall k. k -> Type) ct. Scoped expr patc tw ct (Scope expr patc tw ct)
pScope = MkScoped ask

pLocalScope ::
       forall expr patc (tw :: forall k. k -> Type) ct a.
       (Scope expr patc tw ct -> Scope expr patc tw ct)
    -> Scoped expr patc tw ct a
    -> Scoped expr patc tw ct a
pLocalScope maptc (MkScoped ma) = MkScoped $ local maptc ma

newtype SourceScoped expr patc (tw :: forall k. k -> Type) ct a =
    MkSourceScoped (ReaderT SourcePos (Scoped expr patc tw ct) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

askSourcePos :: forall expr patc (tw :: forall k. k -> Type) ct. SourceScoped expr patc tw ct SourcePos
askSourcePos = MkSourceScoped ask

runSourcePos ::
       forall expr patc (tw :: forall k. k -> Type) ct a.
       SourcePos
    -> SourceScoped expr patc tw ct a
    -> Scoped expr patc tw ct a
runSourcePos spos (MkSourceScoped ma) = runReaderT ma spos

liftSourcePos ::
       forall expr patc (tw :: forall k. k -> Type) ct a. Scoped expr patc tw ct a -> SourceScoped expr patc tw ct a
liftSourcePos ma = MkSourceScoped $ lift ma

remonadSourcePos ::
       forall expr1 patc1 (tw1 :: forall k. k -> Type) ct1 expr2 patc2 (tw2 :: forall k. k -> Type) ct2 b.
       (forall a. Scoped expr1 patc1 tw1 ct1 a -> Scoped expr2 patc2 tw2 ct2 a)
    -> SourceScoped expr1 patc1 tw1 ct1 b
    -> SourceScoped expr2 patc2 tw2 ct2 b
remonadSourcePos mm (MkSourceScoped mb) = MkSourceScoped $ remonad mm mb

mapSourcePos ::
       forall expr patc (tw :: forall k. k -> Type) ct a b.
       SourcePos
    -> (SourceScoped expr patc tw ct a -> SourceScoped expr patc tw ct b)
    -> Scoped expr patc tw ct a
    -> Scoped expr patc tw ct b
mapSourcePos spos f ca = runSourcePos spos $ f $ liftSourcePos ca

runSourceScoped ::
       forall expr patc (tw :: forall k. k -> Type) ct a.
       SourcePos
    -> SourceScoped expr patc tw ct a
    -> InterpretResult a
runSourceScoped spos spa = runScoped $ runSourcePos spos spa

spScope :: forall expr patc (tw :: forall k. k -> Type) ct. SourceScoped expr patc tw ct (Scope expr patc tw ct)
spScope = MkSourceScoped $ lift pScope

instance forall expr patc (tw :: forall k. k -> Type) ct. MonadThrow ExpressionError (SourceScoped expr patc tw ct) where
    throw err = throw $ ExpressionErrorError err

instance forall expr patc (tw :: forall k. k -> Type) ct. MonadThrow PinaforeError (SourceScoped expr patc tw ct) where
    throw err = MkSourceScoped $ throw err

instance forall expr patc (tw :: forall k. k -> Type) ct. MonadThrow ErrorMessage (SourceScoped expr patc tw ct) where
    throw err = MkSourceScoped $ throw err

instance forall expr patc (tw :: forall k. k -> Type) ct. MonadThrow ErrorType (SourceScoped expr patc tw ct) where
    throw err =
        MkSourceScoped $ do
            spos <- ask
            throw $ MkErrorMessage spos err

convertFailure :: forall expr patc (tw :: forall k. k -> Type) ct a. Text -> Text -> SourceScoped expr patc tw ct a
convertFailure ta tb = throw $ TypeConvertError ta tb

lookupBinding :: forall expr patc (tw :: forall k. k -> Type) ct. Name -> SourceScoped expr patc tw ct (Maybe expr)
lookupBinding name = do
    (scopeBindings -> names) <- spScope
    return $ lookup name names

withNewBindings ::
       forall expr patc (tw :: forall k. k -> Type) ct a.
       StrictMap Name expr
    -> Scoped expr patc tw ct a
    -> Scoped expr patc tw ct a
withNewBindings bb ma = pLocalScope (\tc -> tc {scopeBindings = bb <> (scopeBindings tc)}) ma

lookupNamedTypeM ::
       forall expr patc (tw :: forall k. k -> Type) ct. Name -> SourceScoped expr patc tw ct (Maybe (NamedType tw ct))
lookupNamedTypeM name = do
    (scopeTypes -> names) <- spScope
    return $ lookup name names

lookupNamedType ::
       forall expr patc (tw :: forall k. k -> Type) ct. Name -> SourceScoped expr patc tw ct (NamedType tw ct)
lookupNamedType name = do
    mnt <- lookupNamedTypeM name
    case mnt of
        Just nt -> return nt
        Nothing -> throw $ LookupTypeUnknownError name

lookupPatternConstructorM ::
       forall expr patc (tw :: forall k. k -> Type) ct. Name -> SourceScoped expr patc tw ct (Maybe patc)
lookupPatternConstructorM name = do
    (scopePatternConstructors -> names) <- spScope
    return $ lookup name names

lookupPatternConstructor :: forall expr patc (tw :: forall k. k -> Type) ct. Name -> SourceScoped expr patc tw ct patc
lookupPatternConstructor name = do
    ma <- lookupPatternConstructorM name
    case ma of
        Just a -> return a
        Nothing -> throw $ LookupConstructorUnknownError name

newTypeID :: forall expr patc (tw :: forall k. k -> Type) ct. SourceScoped expr patc tw ct TypeID
newTypeID =
    liftSourcePos $
    MkScoped $
    lift $ do
        tid <- get
        put $ succTypeID tid
        return tid

registerTypeName ::
       forall expr patc (tw :: forall k. k -> Type) ct.
       Name
    -> NamedType tw ct
    -> SourceScoped expr patc tw ct (WMFunction (Scoped expr patc tw ct) (Scoped expr patc tw ct))
registerTypeName name t = do
    mnt <- lookupNamedTypeM name
    case mnt of
        Just _ -> throw $ DeclareTypeDuplicateError name
        Nothing -> return $ (MkWMFunction $ pLocalScope (\tc -> tc {scopeTypes = insertMap name t (scopeTypes tc)}))

withNewPatternConstructor ::
       forall expr patc (tw :: forall k. k -> Type) ct.
       Name
    -> patc
    -> SourceScoped expr patc tw ct (WMFunction (Scoped expr patc tw ct) (Scoped expr patc tw ct))
withNewPatternConstructor name pc = do
    ma <- lookupPatternConstructorM name
    case ma of
        Just _ -> throw $ DeclareConstructorDuplicateError name
        Nothing ->
            return $
            MkWMFunction $
            pLocalScope (\tc -> tc {scopePatternConstructors = insertMap name pc $ scopePatternConstructors tc})

withNewPatternConstructors ::
       forall expr patc (tw :: forall k. k -> Type) ct a.
       StrictMap Name patc
    -> Scoped expr patc tw ct a
    -> Scoped expr patc tw ct a
withNewPatternConstructors pp = pLocalScope (\tc -> tc {scopePatternConstructors = pp <> scopePatternConstructors tc})

withEntitySubtype ::
       forall expr patc (tw :: forall k. k -> Type) ct tida tidb.
       TypeIDType tida
    -> TypeIDType tidb
    -> SourceScoped expr patc tw ct (WMFunction (Scoped expr patc tw ct) (Scoped expr patc tw ct))
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
       forall expr patc (tw :: forall k. k -> Type) ct tida tidb.
       Name
    -> TypeIDType tida
    -> Name
    -> TypeIDType tidb
    -> SourceScoped expr patc tw ct (JMShim (OpenEntity tida) (OpenEntity tidb))
getOpenEntitySubtype na wa nb wb = do
    (scopeOpenEntitySubtypes -> subtypes) <- spScope
    case unSubtypeMatch (getSubtypeShim subtypes equalSubtypeMatch) wa wb of
        Just (MkLiftedCategory conv) -> return conv
        Nothing -> convertFailure (exprShow na) (exprShow nb)

class TypeCheckSubtype w where
    getSubtype ::
           forall expr patc (tw :: forall k. k -> Type) ct a b. w a -> w b -> SourceScoped expr patc tw ct (a -> b)
