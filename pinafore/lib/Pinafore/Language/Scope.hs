module Pinafore.Language.Scope
    ( ScopeGroundType
    , ScopeExpression
    , ScopePatternConstructor
    , ScopeProvidedType
    , ScopeClosedEntityType
    , NamedType(..)
    , Scoped
    , runScoped
    , liftScoped
    , Scope
    , exportScope
    , importModule
    , SourcePos
    , SourceScoped
    , askSourcePos
    , localSourcePos
    , remonadSourcePos
    , runSourcePos
    , liftSourcePos
    , mapSourcePos
    , convertFailure
    , SpecialVals(..)
    , getSpecialVals
    , lookupLetBinding
    , withNewLetBindings
    , withRemovedBindings
    , lookupSpecialForm
    , withNewSpecialForms
    , lookupNamedType
    , newTypeID
    , TypeBox(..)
    , registerTypeNames
    , lookupPatternConstructor
    , withNewPatternConstructor
    , withNewPatternConstructors
    , withSubtypeConversions
    , getSubtypeConversions
    ) where

import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Name
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type.DynamicEntity
import Pinafore.Language.Type.Identified
import Pinafore.Language.Type.Show
import Shapes
import Text.Parsec (SourcePos)

type family ScopeGroundType (ts :: Type) :: GroundTypeKind

type family ScopeExpression (ts :: Type) :: Type

type family ScopePatternConstructor (ts :: Type) :: Type

type family ScopeProvidedType (ts :: Type) :: forall k. k -> Type

type family ScopeClosedEntityType (ts :: Type) :: Type -> Type

data NamedType (ts :: Type) where
    SimpleNamedType
        :: forall (ts :: Type) (dv :: DolanVariance) (t :: DolanVarianceKind dv).
           DolanVarianceType dv
        -> DolanVarianceMap dv t
        -> ListTypeExprShow dv
        -> ScopeProvidedType ts t
        -> NamedType ts
    OpenEntityNamedType :: TypeID -> NamedType ts
    ClosedEntityNamedType
        :: forall (ts :: Type) (tid :: BigNat).
           TypeIDType tid
        -> ScopeClosedEntityType ts (Identified tid)
        -> NamedType ts
    DynamicEntityNamedType :: forall (ts :: Type). DynamicEntityType -> NamedType ts

newtype SpecialVals (ts :: Type) = MkSpecialVals
    { specialEvaluate :: forall t. TSPosWitness ts t -> Text -> PinaforeAction (Either Text t)
        -- ^ in Action because this can do things like import files
    }

data ScopeBinding (ts :: Type)
    = ValueBinding (ScopeExpression ts)
                   (Maybe (ScopePatternConstructor ts))
    | TypeBinding (NamedType ts)
    | SpecialFormBinding (SpecialForm ts (SourceScoped ts))

data Scope (ts :: Type) = MkScope
    { scopeBindings :: Map Name (ScopeBinding ts)
    , scopeSubtypes :: HashMap Unique (SubypeConversionEntry (ScopeGroundType ts))
    }

instance Semigroup (Scope ts) where
    MkScope b1 s1 <> MkScope b2 s2 = MkScope (b1 <> b2) (s1 <> s2)

instance Monoid (Scope ts) where
    mempty = MkScope mempty mempty

type InterpretContext :: Type -> Type
data InterpretContext ts = MkInterpretContext
    { icScope :: Scope ts
    , icSpecialVals :: SpecialVals ts
    , icModulePath :: [ModuleName]
    , icLoadModule :: ModuleName -> Scoped ts (Maybe (Scope ts))
    }

type InterpretState :: Type -> Type
data InterpretState ts = MkInterpretState
    { isTypeID :: TypeID
    , isModules :: Map ModuleName (Scope ts)
    }

emptyInterpretState :: InterpretState ts
emptyInterpretState = let
    isTypeID = zeroTypeID
    isModules = mempty
    in MkInterpretState {..}

type Scoped :: Type -> Type -> Type
newtype Scoped ts a = MkScoped
    { unScoped :: ReaderT (InterpretContext ts) (StateT (InterpretState ts) InterpretResult) a
    } deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, MonadFix)

instance MonadThrow PinaforeError (Scoped ts) where
    throw err = MkScoped $ throw err

instance MonadThrow ErrorMessage (Scoped ts) where
    throw = throwErrorMessage

instance Semigroup a => Semigroup (Scoped ts a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Scoped ts a) where
    mappend = (<>)
    mempty = pure mempty

runScoped :: (ModuleName -> Scoped ts (Maybe (Scope ts))) -> SpecialVals ts -> Scoped ts a -> InterpretResult a
runScoped loadModule spvals qa =
    evalStateT (runReaderT (unScoped qa) $ MkInterpretContext mempty spvals [] loadModule) emptyInterpretState

liftScoped :: InterpretResult a -> Scoped ts a
liftScoped ra = MkScoped $ lift $ lift ra

pScope :: Scoped ts (Scope ts)
pScope = MkScoped $ asks icScope

exportScope :: forall ts. [Name] -> SourceScoped ts (Scope ts)
exportScope names = do
    MkScope bindings subtypes <- spScope
    let
        mapName :: Name -> Either Name (Name, ScopeBinding ts)
        mapName name =
            case lookup name bindings of
                Just b -> Right (name, b)
                Nothing -> Left name
        (badnames, goodbinds) = partitionEithers $ fmap mapName names
    case badnames of
        [] -> return $ MkScope (mapFromList goodbinds) subtypes
        (n:nn) -> throw $ LookupNamesUnknownError $ n :| nn

pLocalScope :: (Scope ts -> Scope ts) -> Scoped ts a -> Scoped ts a
pLocalScope maptc (MkScoped ma) = MkScoped $ local (\ic -> ic {icScope = maptc $ icScope ic}) ma

getCycle :: ModuleName -> [ModuleName] -> Maybe (NonEmpty ModuleName)
getCycle _ [] = Nothing
getCycle mn (n:nn)
    | mn == n = Just $ n :| nn
getCycle mn (_:nn) = getCycle mn nn

loadModuleInScope :: forall ts. ModuleName -> Scoped ts (Maybe (Scope ts))
loadModuleInScope mname = do
    oldic <- MkScoped ask
    let
        newic :: InterpretContext ts
        newic = oldic {icScope = mempty, icModulePath = icModulePath oldic <> [mname]}
    MkScoped $ lift $ runReaderT (unScoped (icLoadModule newic mname)) newic

getModule :: ModuleName -> SourceScoped ts (Scope ts)
getModule mname = do
    istate <- liftSourcePos $ MkScoped $ lift get
    let oldmodules = isModules istate
    case lookup mname oldmodules of
        Just m -> return m
        Nothing -> do
            mpath <- liftSourcePos $ MkScoped $ asks icModulePath
            case getCycle mname mpath of
                Just mnames -> throw $ ModuleCycleError mnames
                Nothing -> do
                    mm <- liftSourcePos $ loadModuleInScope mname
                    case mm of
                        Just m -> do
                            liftSourcePos $ MkScoped $ lift $ put istate {isModules = insertMap mname m oldmodules}
                            return m
                        Nothing -> throw $ ModuleNotFoundError mname

importModule :: ModuleName -> SourceScoped ts (WMFunction (Scoped ts) (Scoped ts))
importModule mname = do
    newscope <- getModule mname
    return $ MkWMFunction $ pLocalScope $ \oldscope -> newscope <> oldscope

newtype SourceScoped ts a =
    MkSourceScoped (ReaderT SourcePos (Scoped ts) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, MonadFix)

askSourcePos :: SourceScoped ts SourcePos
askSourcePos = MkSourceScoped ask

localSourcePos :: SourcePos -> SourceScoped ts a -> SourceScoped ts a
localSourcePos spos (MkSourceScoped ma) = MkSourceScoped $ local (\_ -> spos) ma

runSourcePos :: SourcePos -> SourceScoped ts a -> Scoped ts a
runSourcePos spos (MkSourceScoped ma) = runReaderT ma spos

liftSourcePos :: Scoped ts a -> SourceScoped ts a
liftSourcePos ma = MkSourceScoped $ lift ma

remonadSourcePos :: (forall a. Scoped p1 a -> Scoped p2 a) -> SourceScoped p1 b -> SourceScoped p2 b
remonadSourcePos mm (MkSourceScoped mb) = MkSourceScoped $ remonad mm mb

mapSourcePos :: forall ts a b. SourcePos -> (SourceScoped ts a -> SourceScoped ts b) -> Scoped ts a -> Scoped ts b
mapSourcePos spos f ca = runSourcePos spos $ f $ liftSourcePos ca

spScope :: SourceScoped ts (Scope ts)
spScope = liftSourcePos pScope

instance MonadThrow ExpressionError (SourceScoped ts) where
    throw err = throw $ ExpressionErrorError err

instance MonadThrow PinaforeError (SourceScoped ts) where
    throw err = MkSourceScoped $ throw err

instance MonadThrow ErrorMessage (SourceScoped ts) where
    throw err = MkSourceScoped $ throw err

instance MonadThrow ErrorType (SourceScoped ts) where
    throw err =
        MkSourceScoped $ do
            spos <- ask
            throw $ MkErrorMessage spos err

convertFailure :: Text -> Text -> SourceScoped ts a
convertFailure ta tb = throw $ TypeConvertError ta tb

withNewBinding :: Name -> ScopeBinding ts -> Scoped ts a -> Scoped ts a
withNewBinding name b = pLocalScope $ \tc -> tc {scopeBindings = insertMapLazy name b $ scopeBindings tc}

withNewBindings :: Map Name (ScopeBinding ts) -> Scoped ts a -> Scoped ts a
withNewBindings bb = pLocalScope $ \tc -> tc {scopeBindings = bb <> (scopeBindings tc)}

withRemovedBindings :: [Name] -> Scoped ts a -> Scoped ts a
withRemovedBindings nn = pLocalScope $ \tc -> tc {scopeBindings = deletesMap nn $ scopeBindings tc}

lookupBinding :: Name -> SourceScoped ts (Maybe (ScopeBinding ts))
lookupBinding name = do
    (scopeBindings -> names) <- spScope
    return $ lookup name names

getSpecialVals :: SourceScoped ts (SpecialVals ts)
getSpecialVals = liftSourcePos $ MkScoped $ asks icSpecialVals

lookupLetBinding :: Name -> SourceScoped ts (Maybe (ScopeExpression ts))
lookupLetBinding name = do
    mb <- lookupBinding name
    case mb of
        Just (ValueBinding exp _) -> return $ Just exp
        _ -> return Nothing

withNewLetBindings :: Map Name (ScopeExpression ts) -> Scoped ts a -> Scoped ts a
withNewLetBindings bb = withNewBindings $ fmap (\exp -> ValueBinding exp Nothing) bb

lookupSpecialForm :: Name -> SourceScoped ts (SpecialForm ts (SourceScoped ts))
lookupSpecialForm name = do
    mb <- lookupBinding name
    case mb of
        Just (SpecialFormBinding sf) -> return sf
        _ -> throw $ LookupSpecialFormUnknownError name

withNewSpecialForms :: Map Name (SpecialForm ts (SourceScoped ts)) -> Scoped ts a -> Scoped ts a
withNewSpecialForms bb = withNewBindings $ fmap SpecialFormBinding bb

lookupNamedTypeM :: Name -> SourceScoped ts (Maybe (NamedType ts))
lookupNamedTypeM name = do
    mb <- lookupBinding name
    return $
        case mb of
            Just (TypeBinding t) -> Just t
            _ -> Nothing

lookupNamedType :: Name -> SourceScoped ts (NamedType ts)
lookupNamedType name = do
    mnt <- lookupNamedTypeM name
    case mnt of
        Just nt -> return nt
        Nothing -> throw $ LookupTypeUnknownError name

lookupPatternConstructorM :: Name -> SourceScoped ts (Maybe (ScopePatternConstructor ts))
lookupPatternConstructorM name = do
    mb <- lookupBinding name
    return $
        case mb of
            Just (ValueBinding _ (Just pc)) -> Just pc
            _ -> Nothing

lookupPatternConstructor :: Name -> SourceScoped ts (ScopePatternConstructor ts)
lookupPatternConstructor name = do
    ma <- lookupPatternConstructorM name
    case ma of
        Just a -> return a
        Nothing -> throw $ LookupConstructorUnknownError name

newTypeID :: Scoped ts TypeID
newTypeID =
    MkScoped $
    lift $ do
        istate <- get
        let tid = isTypeID istate
        put $ istate {isTypeID = succTypeID tid}
        return tid

registerType :: Name -> NamedType ts -> WMFunction (SourceScoped ts) (SourceScoped ts)
registerType name t =
    MkWMFunction $ \mta -> do
        mnt <- lookupNamedTypeM name
        case mnt of
            Just _ -> throw $ DeclareTypeDuplicateError name
            Nothing -> remonadSourcePos (withNewBinding name $ TypeBinding t) mta

data TypeBox ts x =
    forall t. MkTypeBox Name
                        (t -> NamedType ts)
                        (SourceScoped ts (t, x))

typeBoxToFixBox :: TypeBox ts x -> FixBox (WriterT [x] (SourceScoped ts))
typeBoxToFixBox (MkTypeBox name ttype mtx) =
    mkFixBox (\t -> liftWMFunction $ registerType name $ ttype t) $ do
        (t, x) <- lift mtx
        tell [x]
        return t

registerTypeNames :: [TypeBox ts x] -> SourceScoped ts (WMFunction (Scoped ts) (Scoped ts), [x])
registerTypeNames tboxes = do
    (sc, xx) <- runWriterT $ boxFix (fmap typeBoxToFixBox tboxes) $ lift spScope
    return (MkWMFunction $ pLocalScope (\_ -> sc), xx)

withNewPatternConstructor ::
       Name -> ScopeExpression ts -> ScopePatternConstructor ts -> SourceScoped ts (WMFunction (Scoped ts) (Scoped ts))
withNewPatternConstructor name exp pc = do
    ma <- lookupPatternConstructorM name
    case ma of
        Just _ -> throw $ DeclareConstructorDuplicateError name
        Nothing -> return $ MkWMFunction $ withNewBinding name $ ValueBinding exp $ Just pc

withNewPatternConstructors :: Map Name (ScopeExpression ts, ScopePatternConstructor ts) -> Scoped ts a -> Scoped ts a
withNewPatternConstructors pp = withNewBindings $ fmap (\(exp, pc) -> ValueBinding exp $ Just pc) pp

withSubtypeConversions :: [SubypeConversionEntry (ScopeGroundType ts)] -> Scoped ts a -> Scoped ts a
withSubtypeConversions newscs ma = do
    pairs <-
        liftIO $
        for newscs $ \newsc -> do
            key <- newUnique
            return (key, newsc)
    pLocalScope (\tc -> tc {scopeSubtypes = mapFromList pairs <> scopeSubtypes tc}) ma

getSubtypeConversions :: Scoped ts [SubypeConversionEntry (ScopeGroundType ts)]
getSubtypeConversions = fmap (fmap snd . mapToList . scopeSubtypes) pScope
