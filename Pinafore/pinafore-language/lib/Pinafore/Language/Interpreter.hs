{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Interpreter
    ( runInterpreter
    , allocateVar
    , QModule(..)
    , QScope
    , emptyScope
    , joinScopes
    , joinAllScopes
    , registerScope
    , exportScope
    , getModule
    , SourcePos
    , initialPos
    , QInterpreter
    , nameWitnessErrorType
    , QScopeInterpreter
    , sourcePosParam
    , scopeSourcePos
    , getCurrentNamespace
    , withCurrentNamespace
    , withCurrentNamespaceScope
    , usingNamespace
    , getRenderFullName
    , QSpecialVals(..)
    , getSpecialVals
    , getBindingInfoMap
    , lookupMaybeValue
    , lookupBoundType
    , lookupPatternConstructor
    , lookupRecordConstructor
    , lookupSpecialForm
    , QBoundValue(..)
    , lookupBoundValue
    , registerLetBindings
    , registerLetBinding
    , getSubtypeScope
    , newTypeID
    , withNewTypeID
    , registerBoundType
    , registerType
    , QFixBox
    , QSignature(..)
    , QRecordConstructor(..)
    , QSpecialForm(..)
    , QBindingInfo(..)
    , bindingInfosToScope
    , QInterpreterBinding(..)
    , registerMatchBindings
    , registerPatternConstructor
    , registerRecord
    , registerSubtypeConversion
    , lookupDebugBindingInfo
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.DefDoc
import Pinafore.Language.Error
import Pinafore.Language.Name
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Identified
import Pinafore.Language.Type.Subtype ()
import Pinafore.Language.VarID
import Pinafore.Text
import Shapes
import Text.Parsec.Pos (SourcePos, initialPos)

newtype QSpecialVals = MkQSpecialVals
    { specialEvaluate :: forall t. QType 'Positive t -> Text -> Action (Either Text t)
        -- ^ in Action because this can do things like import files
    }

data QSignature (polarity :: Polarity) (t :: Type) =
    ValueSignature TypeID
                   Name
                   (QType polarity t)
                   (Maybe (QOpenExpression t))

instance Is PolarityType polarity => HasVarMapping (QSignature polarity) where
    getVarMapping (ValueSignature _ _ t _) = getVarMapping t

data QRecordConstructor =
    forall (t :: Type) (tt :: [Type]). MkQRecordConstructor (ListVType (QSignature 'Positive) tt)
                                                            (QGroundedShimWit 'Positive t)
                                                            (QGroundedShimWit 'Negative t)
                                                            (Codec t (ListVProduct tt))

type QSpecialForm :: Type
data QSpecialForm =
    forall lt. MkQSpecialForm (ListType QAnnotation lt)
                              (ListProduct lt -> QInterpreter QValue)

data QInterpreterBinding
    = ValueBinding QExpression
                   (Maybe QPatternConstructor)
    | TypeBinding QSomeGroundType
    | RecordConstructorBinding QRecordConstructor
    | SpecialFormBinding QSpecialForm

instance Show QInterpreterBinding where
    show (ValueBinding e Nothing) = "val: " <> show e
    show (ValueBinding e (Just _)) = "val+pat: " <> show e
    show (TypeBinding t) = "type: " <> unpack (toText $ exprShow t)
    show (RecordConstructorBinding _) = "recordcons"
    show (SpecialFormBinding _) = "special"

data QBindingInfo = MkQBindingInfo
    { biOriginalName :: FullName
    , biDocumentation :: RawMarkdown
    , biValue :: QInterpreterBinding
    }

newtype NameMap =
    MkNameMap (Map FullName QBindingInfo)

instance Semigroup NameMap where
    MkNameMap nsa <> MkNameMap nsb = let
        joinBindings _ bb = bb
        in MkNameMap $ unionWith joinBindings nsa nsb

instance Monoid NameMap where
    mempty = MkNameMap mempty

instance Show NameMap where
    show (MkNameMap m) =
        "{" <> intercalate "," (fmap (\(n, b) -> show n <> "=" <> show (biValue b)) $ mapToList m) <> "}"

nameMapLookupNamespace :: Namespace -> Namespace -> (FullNameRef -> Bool) -> NameMap -> NameMap
nameMapLookupNamespace sourcens destns ff (MkNameMap nm) = let
    matchNS :: forall a. (FullName, a) -> Maybe (FullName, a)
    matchNS (fn, a) = do
        fnr <- namespaceWithinFullNameRef sourcens fn
        altIf $ ff fnr
        return (namespaceConcatFullName destns fnr, a)
    newEntries = mapMaybe matchNS $ mapToList nm
    in MkNameMap $ mapFromList newEntries <> nm

bindingInfoToNameMap :: (FullName, QBindingInfo) -> NameMap
bindingInfoToNameMap (fname, bi) = MkNameMap $ singletonMap fname bi

bindingInfosToNameMap :: [(FullName, QBindingInfo)] -> NameMap
bindingInfosToNameMap bis = mconcat $ fmap bindingInfoToNameMap bis

bindingInfosToScope :: [(FullName, QBindingInfo)] -> QScope
bindingInfosToScope bis = emptyScope {scopeBindings = bindingInfosToNameMap bis}

data QScope = MkQScope
    { scopeBindings :: NameMap
    , scopeSubtypes :: HashMap Unique (SubtypeConversionEntry QGroundType)
    }

emptyScope :: QScope
emptyScope = MkQScope mempty mempty

checkEntryConsistency ::
       SubtypeConversionEntry QGroundType -> HashMap Unique (SubtypeConversionEntry QGroundType) -> QInterpreter ()
checkEntryConsistency sce entries =
    case checkSubtypeConsistency (toList entries) sce of
        Nothing -> return ()
        Just (gta, gtb) -> throw $ InterpretSubtypeInconsistent (exprShow gta) (exprShow gtb)

addSCEntry ::
       (Unique, SubtypeConversionEntry QGroundType)
    -> HashMap Unique (SubtypeConversionEntry QGroundType)
    -> QInterpreter (HashMap Unique (SubtypeConversionEntry QGroundType))
addSCEntry (key, _) entries
    | member key entries = return entries
addSCEntry (key, entry) entries = do
    checkEntryConsistency entry entries
    return $ insertMap key entry entries

addSCEntries ::
       [(Unique, SubtypeConversionEntry QGroundType)]
    -> HashMap Unique (SubtypeConversionEntry QGroundType)
    -> QInterpreter (HashMap Unique (SubtypeConversionEntry QGroundType))
addSCEntries [] entries = return entries
addSCEntries (a:aa) entries = do
    entries' <- addSCEntry a entries
    addSCEntries aa entries'

joinScopes :: QScope -> QScope -> QInterpreter (QScope)
joinScopes a b = do
    let
        bb = scopeBindings b <> scopeBindings a
        alist = mapToList $ scopeSubtypes a
    st <- addSCEntries alist $ scopeSubtypes b
    return MkQScope {scopeBindings = bb, scopeSubtypes = st}

joinAllScopes :: [QScope] -> QScope -> QInterpreter (QScope)
joinAllScopes [] s = return s
joinAllScopes (a:aa) s = do
    s' <- joinScopes a s
    joinAllScopes aa s'

data QModule = MkQModule
    { moduleDoc :: Tree DefDoc
    , moduleScope :: QScope
    }

data InterpretContext = MkInterpretContext
    { icSourcePos :: SourcePos
    , icVarIDState :: VarIDState
    , icScope :: QScope
    , icCurrentNamespace :: Namespace
    , icSpecialVals :: QSpecialVals
    , icModulePath :: [ModuleName]
    , icLoadModule :: ModuleName -> QInterpreter (Maybe QModule)
    }

data InterpretState = MkInterpretState
    { isTypeID :: TypeID
    , isModules :: Map ModuleName QModule
    }

emptyInterpretState :: InterpretState
emptyInterpretState = let
    isTypeID = zeroTypeID
    isModules = mempty
    in MkInterpretState {..}

type QInterpreter :: Type -> Type
newtype QInterpreter a = MkQInterpreter
    { unInterpreter :: ReaderT InterpretContext (StateT InterpretState InterpretResult) a
    } deriving ( Functor
               , Applicative
               , Alternative
               , Monad
               , MonadIO
               , MonadPlus
               , MonadFix
               , MonadException
               , MonadThrow PinaforeError
               , MonadCatch PinaforeError
               , MonadThrow ErrorMessage
               , MonadHoistIO
               , MonadTunnelIO
               )

instance MonadThrow ErrorType QInterpreter where
    throw err = do
        em <- mkErrorMessage
        throw $ em err mempty

instance MonadThrow PatternError QInterpreter where
    throw err = throw $ PatternErrorError err

nameWitnessErrorType :: NonEmpty (Some (NameWitness VarID (QShimWit 'Negative))) -> ErrorType
nameWitnessErrorType ww = let
    nwToPair :: Some (NameWitness VarID (QShimWit 'Negative)) -> (FullNameRef, NamedText)
    nwToPair (MkSome (MkNameWitness varid (MkShimWit w _))) = let
        name =
            case varIdNameRef varid of
                FailureResult fnr -> fnr
                SuccessResult fn -> fullNameRef fn
        in withAllConstraint @Type @ShowNamedText w (name, showNamedText w)
    in ExpressionErrorError $ fmap nwToPair ww

instance (vw ~ QShimWit 'Negative) => MonadThrow (NamedExpressionError VarID vw) QInterpreter where
    throw (UndefinedBindingsError ww) = throw $ nameWitnessErrorType ww

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
        return $ MkErrorMessage spos ntt
    getSubtypeConversions = fmap (fmap snd . mapToList . scopeSubtypes) $ paramAsk scopeParam

contextParam :: Param QInterpreter InterpretContext
contextParam = MkParam (MkQInterpreter ask) $ \a (MkQInterpreter m) -> MkQInterpreter $ with a m

sourcePosParam :: Param QInterpreter SourcePos
sourcePosParam = lensMapParam (\bfb a -> fmap (\b -> a {icSourcePos = b}) $ bfb $ icSourcePos a) contextParam

varIDStateParam :: Param QInterpreter VarIDState
varIDStateParam = lensMapParam (\bfb a -> fmap (\b -> a {icVarIDState = b}) $ bfb $ icVarIDState a) contextParam

scopeParam :: Param QInterpreter (QScope)
scopeParam = lensMapParam (\bfb a -> fmap (\b -> a {icScope = b}) $ bfb $ icScope a) contextParam

bindingsParam :: Param QInterpreter NameMap
bindingsParam = lensMapParam (\bfb a -> fmap (\b -> a {scopeBindings = b}) $ bfb $ scopeBindings a) scopeParam

currentNamespaceParam :: Param QInterpreter Namespace
currentNamespaceParam =
    lensMapParam (\bfb a -> fmap (\b -> a {icCurrentNamespace = b}) $ bfb $ icCurrentNamespace a) contextParam

specialValsParam :: Param QInterpreter (QSpecialVals)
specialValsParam = lensMapParam (\bfb a -> fmap (\b -> a {icSpecialVals = b}) $ bfb $ icSpecialVals a) contextParam

modulePathParam :: Param QInterpreter [ModuleName]
modulePathParam = lensMapParam (\bfb a -> fmap (\b -> a {icModulePath = b}) $ bfb $ icModulePath a) contextParam

loadModuleParam :: Param QInterpreter (ModuleName -> QInterpreter (Maybe QModule))
loadModuleParam = lensMapParam (\bfb a -> fmap (\b -> a {icLoadModule = b}) $ bfb $ icLoadModule a) contextParam

interpretStateRef :: Ref QInterpreter InterpretState
interpretStateRef = let
    ref = liftRef stateRef
    in MkRef (MkQInterpreter $ refGet ref) $ \a -> MkQInterpreter $ refPut ref a

typeIDRef :: Ref QInterpreter TypeID
typeIDRef = lensMapRef (\bfb a -> fmap (\b -> a {isTypeID = b}) $ bfb $ isTypeID a) interpretStateRef

modulesRef :: Ref QInterpreter (Map ModuleName QModule)
modulesRef = lensMapRef (\bfb a -> fmap (\b -> a {isModules = b}) $ bfb $ isModules a) interpretStateRef

runInterpreter ::
       SourcePos -> (ModuleName -> QInterpreter (Maybe QModule)) -> QSpecialVals -> QInterpreter a -> InterpretResult a
runInterpreter icSourcePos icLoadModule icSpecialVals qa = let
    icVarIDState = firstVarIDState
    icScope = emptyScope
    icModulePath = []
    icCurrentNamespace = RootNamespace
    in evalStateT (runReaderT (unInterpreter qa) $ MkInterpretContext {..}) emptyInterpretState

firstOf :: [a] -> (a -> Maybe b) -> Maybe b
firstOf [] _ = Nothing
firstOf (a:aa) amb =
    case amb a of
        Just b -> Just b
        Nothing -> firstOf aa amb

getCurrentNamespace :: QInterpreter Namespace
getCurrentNamespace = paramAsk currentNamespaceParam

withCurrentNamespace :: Namespace -> QInterpreter --> QInterpreter
withCurrentNamespace = paramWith currentNamespaceParam

namespacePriority :: QInterpreter (NamespaceRef -> [Namespace])
namespacePriority = do
    curns <- getCurrentNamespace
    return $ namespaceConcatRefM $ toList $ namespaceAncestry curns

-- | For error messages and the like, doesn't need to be perfect.
getRenderFullName :: QInterpreter (NamedText -> Text)
getRenderFullName = do
    curns <- getCurrentNamespace
    return $ runRelativeNamedText $ toList $ namespaceAncestry curns

nameMapLookupBindingInfo :: NameMap -> FullName -> Maybe (FullName, QBindingInfo)
nameMapLookupBindingInfo (MkNameMap nspace) name = do
    bi <- lookup name nspace
    return (name, bi)

getBindingInfoMap :: QInterpreter (FullNameRef -> Maybe (FullName, QBindingInfo))
getBindingInfoMap = do
    nspace <- paramAsk bindingsParam
    nsp <- namespacePriority
    return $ \(MkFullNameRef name nsn) ->
        firstOf (nsp nsn) $ \ns -> nameMapLookupBindingInfo nspace $ MkFullName name ns

getBindingMap :: QInterpreter (FullNameRef -> Maybe QInterpreterBinding)
getBindingMap = do
    bindmap <- getBindingInfoMap
    return $ \rname -> fmap (biValue . snd) $ bindmap rname

rnuToInterpreter :: FullNameRef -> Maybe a -> QInterpreter a
rnuToInterpreter _ (Just a) = return a
rnuToInterpreter name Nothing = throw $ LookupNotDefinedError name

lookupBinding :: FullNameRef -> QInterpreter QInterpreterBinding
lookupBinding name = do
    bindmap <- getBindingMap
    rnuToInterpreter name $ bindmap name

lookupDebugBindingInfo :: FullNameRef -> QInterpreter (Maybe (FullName, String))
lookupDebugBindingInfo nameref = do
    bindmap <- getBindingInfoMap
    return $ fmap (\(name, b) -> (name, show $ biValue b)) $ bindmap nameref

lookupBoundType :: FullNameRef -> QInterpreter QSomeGroundType
lookupBoundType name = do
    b <- lookupBinding name
    case b of
        TypeBinding t -> return t
        _ -> throw $ LookupNotTypeError name

lookupPatternConstructor :: FullNameRef -> QInterpreter (Either QPatternConstructor QRecordConstructor)
lookupPatternConstructor name = do
    b <- lookupBinding name
    case b of
        ValueBinding _ (Just pc) -> return $ Left pc
        RecordConstructorBinding rc -> return $ Right rc
        _ -> throw $ LookupNotConstructorError name

lookupRecordConstructor :: FullNameRef -> QInterpreter QRecordConstructor
lookupRecordConstructor name = do
    b <- lookupBinding name
    case b of
        RecordConstructorBinding rc -> return rc
        _ -> throw $ LookupNotRecordConstructorError name

lookupSpecialForm :: FullNameRef -> QInterpreter QSpecialForm
lookupSpecialForm name = do
    b <- lookupBinding name
    case b of
        SpecialFormBinding sf -> return sf
        _ -> throw $ LookupNotSpecialFormError name

data QBoundValue
    = ValueBoundValue QExpression
    | RecordBoundValue QRecordConstructor

lookupBoundValue :: FullNameRef -> QInterpreter (QBoundValue)
lookupBoundValue name = do
    b <- lookupBinding name
    case b of
        ValueBinding exp _ -> return $ ValueBoundValue exp
        RecordConstructorBinding rc -> return $ RecordBoundValue rc
        _ -> throw $ LookupNotConstructorError name

lookupMaybeValue :: FullNameRef -> QInterpreter (Maybe QExpression)
lookupMaybeValue name = do
    mb <- getBindingMap
    return $
        case mb name of
            Just (ValueBinding exp _) -> Just exp
            _ -> Nothing

checkPureExpression :: QExpression -> QInterpreter ()
checkPureExpression expr = do
    _ <- tsEval @QTypeSystem expr
    return ()

checkPureBinding :: QInterpreterBinding -> QInterpreter ()
checkPureBinding (ValueBinding expr _) = checkPureExpression expr
checkPureBinding _ = return ()

exportNames :: [FullNameRef] -> QInterpreter [(FullName, QBindingInfo)]
exportNames names = do
    bindmap <- getBindingInfoMap
    let
        (badnamesl, bis) =
            partitionEithers $
            fmap
                (\name ->
                     case bindmap name of
                         Just bi -> Right bi
                         Nothing -> Left name)
                names
    case nonEmpty badnamesl of
        Just badnames -> throw $ LookupNamesUndefinedError badnames
        Nothing -> return bis

exportNamespace :: [Namespace] -> QInterpreter [(FullName, QBindingInfo)]
exportNamespace cnss = do
    MkNameMap nspace <- paramAsk bindingsParam
    let
        toBI :: (FullName, QBindingInfo) -> Bool
        toBI (MkFullName _ ns, _) = isJust $ choice $ fmap (\cns -> namespaceWithin cns ns) cnss
    return $ filter toBI $ mapToList nspace

exportScope :: [Namespace] -> [FullNameRef] -> QInterpreter ([FullName], QScope)
exportScope nsns names = do
    MkQScope _ subtypes <- paramAsk scopeParam
    nsbindss <- exportNamespace nsns
    nbinds <- exportNames names
    let
        binds :: [(FullName, QBindingInfo)]
        binds = nsbindss <> nbinds
    for_ binds $ \bi -> checkPureBinding $ biValue $ snd bi
    return $ (fmap fst binds, MkQScope (bindingInfosToNameMap binds) subtypes)

type QScopeInterpreter = TransformT QInterpreter

scopeRef :: Ref QScopeInterpreter QScope
scopeRef = transformParamRef scopeParam

bindingsRef :: Ref QScopeInterpreter NameMap
bindingsRef = transformParamRef bindingsParam

currentNamespaceRef :: Ref QScopeInterpreter Namespace
currentNamespaceRef = transformParamRef currentNamespaceParam

varIDStateRef :: Ref QScopeInterpreter VarIDState
varIDStateRef = transformParamRef varIDStateParam

scopeSourcePos :: SourcePos -> QScopeInterpreter ()
scopeSourcePos = refPut (transformParamRef sourcePosParam)

allocateVar :: Maybe FullName -> QScopeInterpreter (FullName, VarID)
allocateVar mname = do
    vs <- refGet varIDStateRef
    let
        (vid, name) =
            case mname of
                Just name' -> (mkVarID vs name', name')
                Nothing -> mkUniqueVarID vs
        biOriginalName = name
        biDocumentation = fromString "variable"
        biValue = ValueBinding (tsVar @QTypeSystem vid) Nothing
        insertScope = MkQScope (bindingInfoToNameMap (name, MkQBindingInfo {..})) mempty
    refPut varIDStateRef $ nextVarIDState vs
    refModifyM scopeRef $ \oldScope -> lift $ joinScopes insertScope oldScope
    return (name, vid)

getRestore :: Monad m => Ref m a -> m (m ())
getRestore r = do
    old <- refGet r
    return $ refPut r old

withCurrentNamespaceScope :: Namespace -> QScopeInterpreter (QScopeInterpreter ())
withCurrentNamespaceScope ns = do
    nrestore <- getRestore currentNamespaceRef
    refPut currentNamespaceRef ns
    return nrestore

usingNamespace :: Namespace -> Namespace -> (FullNameRef -> Bool) -> QScopeInterpreter ()
usingNamespace sourcens destns ff = refModify bindingsRef $ nameMapLookupNamespace sourcens destns ff

registerScope :: QScope -> QScopeInterpreter ()
registerScope insertScope = refModifyM scopeRef $ \oldScope -> lift $ joinScopes insertScope oldScope

getCycle :: ModuleName -> [ModuleName] -> Maybe (NonEmpty ModuleName)
getCycle _ [] = Nothing
getCycle mn (n:nn)
    | mn == n = Just $ n :| nn
getCycle mn (_:nn) = getCycle mn nn

loadModuleInScope :: ModuleName -> QInterpreter (Maybe QModule)
loadModuleInScope mname =
    paramWith sourcePosParam (initialPos "<unknown>") $
    paramWith scopeParam emptyScope $
    paramLocal modulePathParam (\path -> path <> [mname]) $ do
        loadModule <- paramAsk loadModuleParam
        loadModule mname

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

registerBinding :: FullName -> QBindingInfo -> QScopeInterpreter ()
registerBinding name db = registerBindings $ singletonMap name db

getSubtypeScope :: SubtypeConversionEntry QGroundType -> QInterpreter (QScope)
getSubtypeScope sce = do
    key <- liftIO newUnique
    return $ emptyScope {scopeSubtypes = singletonMap key sce}

registerBindings :: [(FullName, QBindingInfo)] -> QScopeInterpreter ()
registerBindings bb = do
    let newBindings = MkNameMap $ mapFromList bb
    refModify bindingsRef $ \oldBindings -> oldBindings <> newBindings

getSpecialVals :: QInterpreter (QSpecialVals)
getSpecialVals = paramAsk specialValsParam

registerLetBindings :: [(FullName, RawMarkdown, QExpression)] -> QScopeInterpreter ()
registerLetBindings bb =
    registerBindings $ fmap (\(fname, doc, exp) -> (fname, MkQBindingInfo fname doc $ ValueBinding exp Nothing)) bb

registerLetBinding :: FullName -> RawMarkdown -> QExpression -> QScopeInterpreter ()
registerLetBinding name doc expr = registerLetBindings $ pure (name, doc, expr)

registerMatchBindings :: TSMatch QTypeSystem -> QScopeInterpreter ()
registerMatchBindings match = do
    let
        rbb =
            for (tsMatchBindings @QTypeSystem match) $ \(wvar, expr) -> do
                vn <- varIdNameRef wvar
                return (vn, "lambda", expr)
    case rbb of
        SuccessResult bb -> registerLetBindings bb
        FailureResult fn -> lift $ throw $ KnownIssueError 0 $ "bad match var: " <> showNamedText fn

newTypeID :: QInterpreter (Some TypeIDType)
newTypeID = do
    tid <- refGet typeIDRef
    refPut typeIDRef $ succTypeID tid
    return $ valueToSome tid

withNewTypeID :: (forall tid. TypeIDType tid -> QInterpreter a) -> QInterpreter a
withNewTypeID call = do
    stid <- newTypeID
    case stid of
        MkSome tid -> call tid

checkName :: FullName -> QScopeInterpreter ()
checkName name = do
    mnt <- lift getBindingMap
    case mnt $ fullNameRef name of
        Just _ -> lift $ throw $ DeclareTypeDuplicateError name
        Nothing -> return ()

registerBoundType :: FullName -> RawMarkdown -> QSomeGroundType -> QScopeInterpreter ()
registerBoundType name doc t = do
    checkName name
    registerBinding name $ MkQBindingInfo name doc $ TypeBinding t

registerType :: forall dv t. FullName -> RawMarkdown -> QGroundType dv t -> QScopeInterpreter ()
registerType name doc t = do
    checkName name
    registerBoundType name doc $ MkSomeGroundType t

type QFixBox = FixBox QScopeInterpreter

registerPatternConstructor :: FullName -> RawMarkdown -> QExpression -> QPatternConstructor -> QScopeInterpreter ()
registerPatternConstructor name doc exp pc = do
    checkName name
    registerBinding name $ MkQBindingInfo name doc $ ValueBinding exp $ Just pc

registerRecord :: FullName -> RawMarkdown -> QRecordConstructor -> QScopeInterpreter ()
registerRecord name doc rc = do
    checkName name
    registerBinding name $ MkQBindingInfo name doc $ RecordConstructorBinding rc

registerSubtypeConversion :: SubtypeConversionEntry QGroundType -> QScopeInterpreter ()
registerSubtypeConversion sce = do
    newscope <- lift $ getSubtypeScope sce
    registerScope newscope
