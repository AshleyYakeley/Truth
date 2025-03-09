{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Interpreter.Interpreter
    ( QInterpreter
    , LoadModule (..)
    , nameWitnessErrorType
    , sourcePosParam
    , varIDStateParam
    , scopeParam
    , loadModuleParam
    , currentNamespaceParam
    , appNotationVarRef
    , appNotationBindsProd
    , LibraryContext (..)
    , runInterpreter
    , getRenderFullName
    , getBindingInfoLookup
    , getNamespaceWithScope
    , exportScope
    , getModule
    , getSubtypeScope
    , newTypeID
    , withNewTypeID
    , withSemiIdentifiedType
    , newIdentifiedType
    )
where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpreter.Binding
import Pinafore.Language.Interpreter.Scope
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Identified
import Pinafore.Language.Type.Subtype ()
import Pinafore.Language.VarID

newtype LoadModule = MkLoadModule
    { runLoadModule :: ModuleName -> QInterpreter (Maybe QModule)
    }

instance Semigroup LoadModule where
    MkLoadModule fma <> MkLoadModule fmb =
        MkLoadModule $ \mname -> do
            mrr <- fma mname
            case mrr of
                Just rr -> return $ Just rr
                Nothing -> fmb mname

instance Monoid LoadModule where
    mempty = MkLoadModule $ \_ -> return Nothing

data InterpretContext = MkInterpretContext
    { icSourcePos :: SourcePos
    , icVarIDState :: VarIDState
    , icScope :: QScope
    , icCurrentNamespace :: Namespace
    , icModulePath :: [ModuleName]
    , icLoadModule :: LoadModule
    }

data InterpretState = MkInterpretState
    { isTypeID :: TypeID
    , isModules :: Map ModuleName QModule
    , isAppNotationVar :: VarIDState
    }

emptyInterpretState :: InterpretState
emptyInterpretState = let
    isTypeID = szero
    isModules = mempty
    isAppNotationVar = szero
    in MkInterpretState{..}

type InterpretOutput = [(VarID, QExpression)]

type QInterpreter :: Type -> Type
newtype QInterpreter a = MkQInterpreter
    { unInterpreter :: ReaderT InterpretContext (WriterT InterpretOutput (StateT InterpretState InterpretResult)) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadFix
        , MonadException
        , MonadThrow QError
        , MonadCatch QError
        , MonadHoistIO
        , MonadTunnelIO
        )

instance RepresentationalRole QInterpreter where
    representationalCoercion MkCoercion = MkCoercion

instance MaybeRepresentational QInterpreter where
    maybeRepresentational = Just Dict

instance HasVariance QInterpreter where
    type VarianceOf QInterpreter = 'Covariance

instance MonadCoroutine QInterpreter where
    coroutineSuspend pqmr =
        hoist MkQInterpreter $ coroutineSuspend $ \pmq -> unInterpreter $ pqmr $ \p -> MkQInterpreter $ pmq p

instance MonadThrow QErrorType QInterpreter where
    throw err = do
        em <- mkErrorMessage
        throw $ em err

instance MonadThrow PatternError QInterpreter where
    throw err = throw $ PatternErrorError err

-- Left if at least one a
splitNonEmpty :: NonEmpty (Either a b) -> Either (NonEmpty a) (NonEmpty b)
splitNonEmpty (Left a :| r) = Left $ a :| lefts r
splitNonEmpty (Right b :| r) =
    case nonEmpty $ lefts r of
        Just na -> Left na
        Nothing -> Right $ b :| rights r

nameWitnessErrorType :: NonEmpty (Some (NameWitness VarID (QShimWit 'Negative))) -> QErrorType
nameWitnessErrorType ww = let
    nwToPair ::
        Some (NameWitness VarID (QShimWit 'Negative)) -> Either (FullNameRef, NamedText) (ImplicitName, NamedText)
    nwToPair (MkSome (MkNameWitness varid (MkShimWit w _))) = let
        tnt = withAllConstraint @Type @ShowNamedText w $ showNamedText w
        in case varIDName varid of
            Left name -> Left (name, tnt)
            Right name -> Right (name, tnt)
    in case splitNonEmpty $ fmap nwToPair ww of
        Left pp -> ExpressionUndefinedError pp
        Right pp -> ExpressionUnimpliedError pp

instance Semigroup a => Semigroup (QInterpreter a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (QInterpreter a) where
    mappend = (<>)
    mempty = pure mempty

instance HasInterpreter where
    type Interpreter = QInterpreter
    mkErrorMessage = do
        spos <- paramAsk sourcePosParam
        ntt <- getRenderFullName
        return $ MkSourceError spos ntt
    getSubtypeConversions = fmap (toList . scopeSubtypes) $ paramAsk scopeParam

contextParam :: Param QInterpreter InterpretContext
contextParam = MkParam (MkQInterpreter ask) $ \a (MkQInterpreter m) -> MkQInterpreter $ with a m

sourcePosParam :: Param QInterpreter SourcePos
sourcePosParam = lensMapParam (\bfb a -> fmap (\b -> a{icSourcePos = b}) $ bfb $ icSourcePos a) contextParam

varIDStateParam :: Param QInterpreter VarIDState
varIDStateParam = lensMapParam (\bfb a -> fmap (\b -> a{icVarIDState = b}) $ bfb $ icVarIDState a) contextParam

scopeParam :: Param QInterpreter QScope
scopeParam = lensMapParam (\bfb a -> fmap (\b -> a{icScope = b}) $ bfb $ icScope a) contextParam

bindingsParam :: Param QInterpreter QBindingMap
bindingsParam = lensMapParam (\bfb a -> fmap (\b -> a{scopeBindings = b}) $ bfb $ scopeBindings a) scopeParam

currentNamespaceParam :: Param QInterpreter Namespace
currentNamespaceParam =
    lensMapParam (\bfb a -> fmap (\b -> a{icCurrentNamespace = b}) $ bfb $ icCurrentNamespace a) contextParam

modulePathParam :: Param QInterpreter [ModuleName]
modulePathParam = lensMapParam (\bfb a -> fmap (\b -> a{icModulePath = b}) $ bfb $ icModulePath a) contextParam

loadModuleParam :: Param QInterpreter LoadModule
loadModuleParam = lensMapParam (\bfb a -> fmap (\b -> a{icLoadModule = b}) $ bfb $ icLoadModule a) contextParam

interpretStateRef :: Ref QInterpreter InterpretState
interpretStateRef = let
    ref = liftRef $ liftRef stateRef
    in MkRef (MkQInterpreter $ refGet ref) $ \a -> MkQInterpreter $ refPut ref a

appNotationBindsProd :: Prod QInterpreter [(VarID, QExpression)]
appNotationBindsProd = let
    prod = liftProd writerProd
    in MkProd (\a -> MkQInterpreter $ prodTell prod a) (\(MkQInterpreter mr) -> MkQInterpreter $ prodCollect prod mr)

typeIDRef :: Ref QInterpreter TypeID
typeIDRef = lensMapRef (\bfb a -> fmap (\b -> a{isTypeID = b}) $ bfb $ isTypeID a) interpretStateRef

modulesRef :: Ref QInterpreter (Map ModuleName QModule)
modulesRef = lensMapRef (\bfb a -> fmap (\b -> a{isModules = b}) $ bfb $ isModules a) interpretStateRef

appNotationVarRef :: Ref QInterpreter VarIDState
appNotationVarRef =
    lensMapRef (\bfb a -> fmap (\b -> a{isAppNotationVar = b}) $ bfb $ isAppNotationVar a) interpretStateRef

data LibraryContext = MkLibraryContext
    { lcLoadModule :: LoadModule
    }

runInterpreter :: SourcePos -> LibraryContext -> QInterpreter a -> InterpretResult a
runInterpreter icSourcePos MkLibraryContext{..} qa = let
    icVarIDState = szero
    icScope = emptyScope
    icModulePath = []
    icCurrentNamespace = RootNamespace
    icLoadModule = lcLoadModule
    in evalStateT (evalWriterT $ runReaderT (unInterpreter qa) $ MkInterpretContext{..}) emptyInterpretState

firstOf :: [a] -> (a -> Maybe b) -> Maybe b
firstOf [] _ = Nothing
firstOf (a : aa) amb =
    case amb a of
        Just b -> Just b
        Nothing -> firstOf aa amb

namespacePriority :: QInterpreter (NamespaceRef -> [Namespace])
namespacePriority = do
    curns <- paramAsk currentNamespaceParam
    return $ namespaceConcatRefM $ toList $ namespaceAncestry curns

-- | For error messages and the like, doesn't need to be perfect.
getRenderFullName :: QInterpreter (NamedText -> Text)
getRenderFullName = do
    curns <- paramAsk currentNamespaceParam
    return $ runRelativeNamedText $ toList $ namespaceAncestry curns

getBindingInfoLookup :: QInterpreter (FullNameRef -> Maybe (FullName, QBindingInfo))
getBindingInfoLookup = do
    nspace <- paramAsk bindingsParam
    nsp <- namespacePriority
    return $ \(MkFullNameRef name nsn) -> firstOf (nsp nsn) $ \ns -> bindingMapLookupInfo nspace $ MkFullName name ns

getNamespaceWithScope :: Namespace -> Namespace -> (FullNameRef -> Bool) -> QInterpreter QScope
getNamespaceWithScope sourcens destns ff = do
    bmap <- paramAsk bindingsParam
    return $ emptyScope{scopeBindings = bindingMapNamespaceWith sourcens destns ff bmap}

exportNames :: [FullNameRef] -> QInterpreter [(FullName, QBindingInfo)]
exportNames names = do
    bindmap <- getBindingInfoLookup
    let
        (badnamesl, bis) =
            partitionEithers
                $ fmap
                    ( \name ->
                        case bindmap name of
                            Just bi -> Right bi
                            Nothing -> Left name
                    )
                    names
    case nonEmpty badnamesl of
        Just badnames -> throw $ LookupNamesUndefinedError badnames
        Nothing -> return bis

exportNamespace :: [Namespace] -> QInterpreter [(FullName, QBindingInfo)]
exportNamespace cnss = do
    bmap <- paramAsk bindingsParam
    let
        toBI :: (FullName, QBindingInfo) -> Bool
        toBI (MkFullName _ ns, _) = isJust $ choice $ fmap (\cns -> namespaceWithin cns ns) cnss
    return $ filter toBI $ bindingMapEntries bmap

exportScope :: [Namespace] -> [FullNameRef] -> QInterpreter (QScope, [DefDoc])
exportScope nsns names = do
    MkQScope _ subtypes <- paramAsk scopeParam
    nsbindss <- exportNamespace nsns
    nbinds <- exportNames names
    let
        binds :: [(FullName, QBindingInfo)]
        binds = nsbindss <> nbinds
    let
        scope = MkQScope (bindingInfosToMap binds) subtypes
        docs = fmap (biDocumentation . snd) binds
    return (scope, docs)

getCycle :: Eq t => t -> [t] -> Maybe (NonEmpty t)
getCycle _ [] = Nothing
getCycle mn (n : nn)
    | mn == n = Just $ n :| nn
getCycle mn (_ : nn) = getCycle mn nn

loadModuleInScope :: ModuleName -> QInterpreter (Maybe QModule)
loadModuleInScope mname =
    paramWith sourcePosParam (initialPos "<unknown>")
        $ paramWith scopeParam emptyScope
        $ paramLocal modulePathParam (\path -> path <> [mname])
        $ do
            loadModule <- paramAsk loadModuleParam
            runLoadModule loadModule mname

getModule :: ModuleName -> QInterpreter QModule
getModule mname = do
    mods <- refGet modulesRef
    case lookup mname mods of
        Just m -> return m
        Nothing -> do
            mpath <- paramAsk modulePathParam
            case getCycle mname mpath of
                Just mnames -> throw $ ModuleCycleError mnames
                Nothing -> do
                    mm <- loadModuleInScope mname
                    case mm of
                        Just m -> do
                            refModify modulesRef $ insertMap mname m
                            return m
                        Nothing -> throw $ ModuleNotFoundError mname

getSubtypeScope :: QSubtypeConversionEntry -> QInterpreter QScope
getSubtypeScope sce = do
    key <- liftIO newUnique
    return $ emptyScope{scopeSubtypes = singletonMap key sce}

newTypeID :: QInterpreter (Some TypeIDType)
newTypeID = do
    tid <- refSucc typeIDRef
    return $ valueToSome tid

withNewTypeID :: (forall tid. TypeIDType tid -> QInterpreter r) -> QInterpreter r
withNewTypeID call = do
    stid <- newTypeID
    case stid of
        MkSome typeID -> call typeID

withSemiIdentifiedType ::
    forall (dv :: CCRVariances) r.
    (forall (gt :: CCRVariancesKind dv). FamilialType gt -> QInterpreter r) ->
    QInterpreter r
withSemiIdentifiedType call =
    withNewTypeID $ \typeID -> do
        Refl <- unsafeIdentifyKind @_ @(CCRVariancesKind dv) typeID
        call $ identifiedFamilialType typeID

newIdentifiedType :: forall (dv :: CCRVariances) (gt :: CCRVariancesKind dv). QInterpreter (FamilialType gt)
newIdentifiedType =
    withNewTypeID $ \typeID -> do
        Refl <- unsafeIdentifyKind @_ @(CCRVariancesKind dv) typeID
        Refl <- unsafeIdentify @_ @gt typeID
        return $ identifiedFamilialType typeID
