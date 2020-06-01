module Pinafore.Language.Scope
    ( ScopeExpression
    , ScopePatternConstructor
    , ScopeProvidedType
    , ScopeClosedEntityType
    , NamedType(..)
    , Scoped
    , runScoped
    , liftScoped
    , SourcePos
    , SourceScoped
    , askSourcePos
    , localSourcePos
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
    , TypeBox(..)
    , registerTypeNames
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
import Pinafore.Language.Shim
import Pinafore.Language.Subtype
import Pinafore.Language.Type.Identified
import Pinafore.Language.TypeSystem.Show
import Pinafore.Language.Value
import Shapes
import Text.Parsec (SourcePos)

type family ScopeExpression (p :: Type) :: Type

type family ScopePatternConstructor (p :: Type) :: Type

type family ScopeProvidedType (p :: Type) :: forall k. k -> Type

type family ScopeClosedEntityType (p :: Type) :: Type -> Type

data NamedType (p :: Type) where
    SimpleNamedType
        :: forall (p :: Type) (dv :: DolanVariance) (t :: DolanVarianceKind dv).
           DolanVarianceType dv
        -> DolanVarianceMap dv t
        -> ListTypeExprShow dv
        -> ScopeProvidedType p t
        -> NamedType p
    OpenEntityNamedType :: TypeID -> NamedType p
    ClosedEntityNamedType
        :: forall (p :: Type) (tid :: BigNat). TypeIDType tid -> ScopeClosedEntityType p (Identified tid) -> NamedType p

type OpenEntityShim = LiftedCategory (PinaforeShim Type) OpenEntity

data Scope (p :: Type) = MkScope
    { scopeBindings :: Map Name (ScopeExpression p)
    , scopePatternConstructors :: Map Name (ScopePatternConstructor p)
    , scopeTypes :: Map Name (NamedType p)
    , scopeOpenEntitySubtypes :: [SubtypeEntry OpenEntityShim TypeIDType]
    }

newtype Scoped (p :: Type) a =
    MkScoped (ReaderT (Scope p) (StateT TypeID InterpretResult) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix)

instance MonadThrow PinaforeError (Scoped p) where
    throw err = MkScoped $ throw err

instance MonadThrow ErrorMessage (Scoped p) where
    throw = throwErrorMessage

instance Semigroup a => Semigroup (Scoped p a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Scoped p a) where
    mappend = (<>)
    mempty = pure mempty

runScoped :: Scoped p a -> InterpretResult a
runScoped (MkScoped qa) = evalStateT (runReaderT qa $ MkScope mempty mempty mempty mempty) zeroTypeID

liftScoped :: InterpretResult a -> Scoped p a
liftScoped ra = MkScoped $ lift $ lift ra

pScope :: Scoped p (Scope p)
pScope = MkScoped ask

pLocalScope :: (Scope p -> Scope p) -> Scoped p a -> Scoped p a
pLocalScope maptc (MkScoped ma) = MkScoped $ local maptc ma

newtype SourceScoped p a =
    MkSourceScoped (ReaderT SourcePos (Scoped p) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix)

askSourcePos :: SourceScoped p SourcePos
askSourcePos = MkSourceScoped ask

localSourcePos :: SourcePos -> SourceScoped p a -> SourceScoped p a
localSourcePos spos (MkSourceScoped ma) = MkSourceScoped $ local (\_ -> spos) ma

runSourcePos :: SourcePos -> SourceScoped p a -> Scoped p a
runSourcePos spos (MkSourceScoped ma) = runReaderT ma spos

liftSourcePos :: Scoped p a -> SourceScoped p a
liftSourcePos ma = MkSourceScoped $ lift ma

remonadSourcePos :: (forall a. Scoped p1 a -> Scoped p2 a) -> SourceScoped p1 b -> SourceScoped p2 b
remonadSourcePos mm (MkSourceScoped mb) = MkSourceScoped $ remonad mm mb

mapSourcePos :: forall p a b. SourcePos -> (SourceScoped p a -> SourceScoped p b) -> Scoped p a -> Scoped p b
mapSourcePos spos f ca = runSourcePos spos $ f $ liftSourcePos ca

runSourceScoped :: SourcePos -> SourceScoped p a -> InterpretResult a
runSourceScoped spos spa = runScoped $ runSourcePos spos spa

spScope :: SourceScoped p (Scope p)
spScope = MkSourceScoped $ lift pScope

instance MonadThrow ExpressionError (SourceScoped p) where
    throw err = throw $ ExpressionErrorError err

instance MonadThrow PinaforeError (SourceScoped p) where
    throw err = MkSourceScoped $ throw err

instance MonadThrow ErrorMessage (SourceScoped p) where
    throw err = MkSourceScoped $ throw err

instance MonadThrow ErrorType (SourceScoped p) where
    throw err =
        MkSourceScoped $ do
            spos <- ask
            throw $ MkErrorMessage spos err

convertFailure :: Text -> Text -> SourceScoped p a
convertFailure ta tb = throw $ TypeConvertError ta tb

lookupBinding :: Name -> SourceScoped p (Maybe (ScopeExpression p))
lookupBinding name = do
    (scopeBindings -> names) <- spScope
    return $ lookup name names

withNewBindings :: Map Name (ScopeExpression p) -> Scoped p a -> Scoped p a
withNewBindings bb = pLocalScope $ \tc -> tc {scopeBindings = bb <> (scopeBindings tc)}

lookupNamedTypeM :: Name -> SourceScoped p (Maybe (NamedType p))
lookupNamedTypeM name = do
    (scopeTypes -> names) <- spScope
    return $ lookup name names

lookupNamedType :: Name -> SourceScoped p (NamedType p)
lookupNamedType name = do
    mnt <- lookupNamedTypeM name
    case mnt of
        Just nt -> return nt
        Nothing -> throw $ LookupTypeUnknownError name

lookupPatternConstructorM :: Name -> SourceScoped p (Maybe (ScopePatternConstructor p))
lookupPatternConstructorM name = do
    (scopePatternConstructors -> names) <- spScope
    return $ lookup name names

lookupPatternConstructor :: Name -> SourceScoped p (ScopePatternConstructor p)
lookupPatternConstructor name = do
    ma <- lookupPatternConstructorM name
    case ma of
        Just a -> return a
        Nothing -> throw $ LookupConstructorUnknownError name

newTypeID :: Scoped p TypeID
newTypeID =
    MkScoped $
    lift $ do
        tid <- get
        put $ succTypeID tid
        return tid

registerType :: Name -> NamedType p -> WMFunction (SourceScoped p) (SourceScoped p)
registerType name t =
    MkWMFunction $ \mta -> do
        mnt <- lookupNamedTypeM name
        case mnt of
            Just _ -> throw $ DeclareTypeDuplicateError name
            Nothing ->
                remonadSourcePos (pLocalScope $ \tc -> tc {scopeTypes = insertMapLazy name t (scopeTypes tc)}) mta

data TypeBox p x =
    forall t. MkTypeBox Name
                        (t -> NamedType p)
                        (SourceScoped p (t, x))

typeBoxToFixBox :: TypeBox p x -> FixBox (WriterT [x] (SourceScoped p))
typeBoxToFixBox (MkTypeBox name ttype mtx) =
    mkFixBox (\t -> liftWMFunction $ registerType name $ ttype t) $ do
        (t, x) <- lift mtx
        tell [x]
        return t

registerTypeNames :: [TypeBox p x] -> SourceScoped p (WMFunction (Scoped p) (Scoped p), [x])
registerTypeNames tboxes = do
    (sc, xx) <- runWriterT $ boxFix (fmap typeBoxToFixBox tboxes) $ lift spScope
    return (MkWMFunction $ pLocalScope (\_ -> sc), xx)

withNewPatternConstructor :: Name -> ScopePatternConstructor p -> SourceScoped p (WMFunction (Scoped p) (Scoped p))
withNewPatternConstructor name pc = do
    ma <- lookupPatternConstructorM name
    case ma of
        Just _ -> throw $ DeclareConstructorDuplicateError name
        Nothing ->
            return $
            MkWMFunction $
            pLocalScope (\tc -> tc {scopePatternConstructors = insertMap name pc $ scopePatternConstructors tc})

withNewPatternConstructors :: Map Name (ScopePatternConstructor p) -> Scoped p a -> Scoped p a
withNewPatternConstructors pp = pLocalScope (\tc -> tc {scopePatternConstructors = pp <> scopePatternConstructors tc})

withEntitySubtype :: TypeIDType tida -> TypeIDType tidb -> Scoped p a -> Scoped p a
withEntitySubtype ta tb =
    pLocalScope $ \tc ->
        tc
            { scopeOpenEntitySubtypes =
                  (MkSubtypeEntry ta tb $ MkLiftedCategory $ coerceEnhanced "open entity subtype") :
                  (scopeOpenEntitySubtypes tc)
            }

getOpenEntitySubtype ::
       Name
    -> TypeIDType tida
    -> Name
    -> TypeIDType tidb
    -> SourceScoped p (PinaforeShim Type (OpenEntity tida) (OpenEntity tidb))
getOpenEntitySubtype na wa nb wb = do
    (scopeOpenEntitySubtypes -> subtypes) <- spScope
    case unSubtypeMatch (getSubtypeShim subtypes equalSubtypeMatch) wa wb of
        Just (MkLiftedCategory conv) -> return conv
        Nothing -> convertFailure (exprShow na) (exprShow nb)

class TypeCheckSubtype w where
    getSubtype :: forall p a b. w a -> w b -> SourceScoped p (a -> b)
