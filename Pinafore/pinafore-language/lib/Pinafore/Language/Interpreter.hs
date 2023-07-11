module Pinafore.Language.Interpreter
    ( InterpreterGroundType
    , InterpreterFamilyType
    , InterpreterBoundType
    , runInterpreter
    , allocateVar
    , EntryDoc
    , Module(..)
    , Scope
    , emptyScope
    , joinScopes
    , joinAllScopes
    , registerScope
    , exportScope
    , getModule
    , SourcePos
    , initialPos
    , Interpreter
    , mkErrorMessage
    , nameWitnessErrorType
    , ScopeInterpreter
    , sourcePosParam
    , scopeSourcePos
    , getCurrentNamespace
    , withCurrentNamespace
    , withCurrentNamespaceScope
    , usingNamespace
    , getRenderFullName
    , SpecialVals(..)
    , getSpecialVals
    , getBindingInfoMap
    , lookupMaybeValue
    , lookupBoundType
    , lookupPatternConstructor
    , lookupRecordConstructor
    , lookupSpecialForm
    , BoundValue(..)
    , lookupBoundValue
    , registerLetBindings
    , registerLetBinding
    , getSubtypeScope
    , newTypeID
    , withNewTypeID
    , registerBoundType
    , registerType
    , ScopeFixBox
    , Signature(..)
    , RecordConstructor(..)
    , BindingInfo(..)
    , bindingInfosToScope
    , InterpreterBinding(..)
    , registerMatchBindings
    , registerPatternConstructor
    , registerRecord
    , registerSubtypeConversion
    , getSubtypeConversions
    , lookupDebugBindingInfo
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Name
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type.Identified
import Pinafore.Language.VarID
import Pinafore.Text
import Shapes
import Text.Parsec.Pos (SourcePos, initialPos)

type family InterpreterGroundType (ts :: Type) :: GroundTypeKind

type instance forall (gt :: GroundTypeKind). InterpreterGroundType (DolanTypeSystem gt) = gt

type family InterpreterFamilyType (ts :: Type) :: forall k. k -> Type

type InterpreterType (ts :: Type) = DolanType (InterpreterGroundType ts)

type InterpreterGroundedShimWit (ts :: Type) (polarity :: Polarity)
     = DolanGroundedShimWit (InterpreterGroundType ts) polarity

type InterpreterBoundType (ts :: Type) = SomeGroundType (InterpreterGroundType ts)

type HasInterpreter ts
     = ( ts ~ DolanTypeSystem (InterpreterGroundType ts)
       , IsDolanFunctionGroundType (InterpreterGroundType ts)
       , IsDolanSubtypeEntriesGroundType (InterpreterGroundType ts)
       , TSVarID ts ~ VarID
       , AllConstraint ShowNamedText (TSNegWitness ts)
       , ExprShow (InterpreterBoundType ts)
       , Show (TSSealedExpression ts))

newtype SpecialVals (ts :: Type) = MkSpecialVals
    { specialEvaluate :: forall t. TSPosWitness ts t -> Text -> Action (Either Text t)
        -- ^ in Action because this can do things like import files
    }

data Signature (ts :: Type) (polarity :: Polarity) (t :: Type) =
    ValueSignature TypeID
                   Name
                   (InterpreterType ts polarity t)
                   (Maybe (TSOpenExpression ts t))

instance (HasInterpreter ts, Is PolarityType polarity) => HasVarMapping (Signature ts polarity) where
    getVarMapping (ValueSignature _ _ t _) = getVarMapping t

data RecordConstructor (ts :: Type) =
    forall (t :: Type) (tt :: [Type]). MkRecordConstructor (ListVType (Signature ts 'Positive) tt)
                                                           (InterpreterGroundedShimWit ts 'Positive t)
                                                           (InterpreterGroundedShimWit ts 'Negative t)
                                                           (Codec t (ListVProduct tt))

data InterpreterBinding (ts :: Type)
    = ValueBinding (TSSealedExpression ts)
                   (Maybe (TSExpressionPatternConstructor ts))
    | TypeBinding (InterpreterBoundType ts)
    | RecordConstructorBinding (RecordConstructor ts)
    | SpecialFormBinding (SpecialForm ts (Interpreter ts))

instance HasInterpreter ts => Show (InterpreterBinding ts) where
    show (ValueBinding e Nothing) = "val: " <> show e
    show (ValueBinding e (Just _)) = "val+pat: " <> show e
    show (TypeBinding t) = "type: " <> unpack (toText $ exprShow t)
    show (RecordConstructorBinding _) = "recordcons"
    show (SpecialFormBinding _) = "special"

data BindingInfo ts = MkBindingInfo
    { biOriginalName :: FullName
    , biDocumentation :: RawMarkdown
    , biValue :: InterpreterBinding ts
    }

newtype NameMap ts =
    MkNameMap (Map FullName (BindingInfo ts))

instance Semigroup (NameMap ts) where
    MkNameMap nsa <> MkNameMap nsb = let
        joinBindings _ bb = bb
        in MkNameMap $ unionWith joinBindings nsa nsb

instance Monoid (NameMap ts) where
    mempty = MkNameMap mempty

instance HasInterpreter ts => Show (NameMap ts) where
    show (MkNameMap m) =
        "{" <> intercalate "," (fmap (\(n, b) -> show n <> "=" <> show (biValue b)) $ mapToList m) <> "}"

nameMapLookupNamespace :: Namespace -> Namespace -> (FullNameRef -> Bool) -> NameMap ts -> NameMap ts
nameMapLookupNamespace sourcens destns ff (MkNameMap nm) = let
    matchNS :: forall a. (FullName, a) -> Maybe (FullName, a)
    matchNS (fn, a) = do
        fnr <- namespaceWithinFullNameRef sourcens fn
        altIf $ ff fnr
        return (namespaceConcatFullName destns fnr, a)
    newEntries = mapMaybe matchNS $ mapToList nm
    in MkNameMap $ mapFromList newEntries <> nm

bindingInfoToNameMap :: (FullName, BindingInfo ts) -> NameMap ts
bindingInfoToNameMap (fname, bi) = MkNameMap $ singletonMap fname bi

bindingInfosToNameMap :: [(FullName, BindingInfo ts)] -> NameMap ts
bindingInfosToNameMap bis = mconcat $ fmap bindingInfoToNameMap bis

bindingInfosToScope :: [(FullName, BindingInfo ts)] -> Scope ts
bindingInfosToScope bis = emptyScope {scopeBindings = bindingInfosToNameMap bis}

data Scope (ts :: Type) = MkScope
    { scopeBindings :: NameMap ts
    , scopeSubtypes :: HashMap Unique (SubtypeConversionEntry (InterpreterGroundType ts))
    }

emptyScope :: Scope ts
emptyScope = MkScope mempty mempty

checkEntryConsistency ::
       forall ts. HasInterpreter ts
    => SubtypeConversionEntry (InterpreterGroundType ts)
    -> HashMap Unique (SubtypeConversionEntry (InterpreterGroundType ts))
    -> Interpreter ts ()
checkEntryConsistency sce entries =
    case checkSubtypeConsistency (toList entries) sce of
        Nothing -> return ()
        Just (gta, gtb) -> throw $ InterpretSubtypeInconsistent (exprShow gta) (exprShow gtb)

addSCEntry ::
       forall ts. HasInterpreter ts
    => (Unique, SubtypeConversionEntry (InterpreterGroundType ts))
    -> HashMap Unique (SubtypeConversionEntry (InterpreterGroundType ts))
    -> Interpreter ts (HashMap Unique (SubtypeConversionEntry (InterpreterGroundType ts)))
addSCEntry (key, _) entries
    | member key entries = return entries
addSCEntry (key, entry) entries = do
    checkEntryConsistency entry entries
    return $ insertMap key entry entries

addSCEntries ::
       forall ts. HasInterpreter ts
    => [(Unique, SubtypeConversionEntry (InterpreterGroundType ts))]
    -> HashMap Unique (SubtypeConversionEntry (InterpreterGroundType ts))
    -> Interpreter ts (HashMap Unique (SubtypeConversionEntry (InterpreterGroundType ts)))
addSCEntries [] entries = return entries
addSCEntries (a:aa) entries = do
    entries' <- addSCEntry a entries
    addSCEntries aa entries'

joinScopes ::
       forall ts. HasInterpreter ts
    => Scope ts
    -> Scope ts
    -> Interpreter ts (Scope ts)
joinScopes a b = do
    let
        bb = scopeBindings b <> scopeBindings a
        alist = mapToList $ scopeSubtypes a
    st <- addSCEntries alist $ scopeSubtypes b
    return MkScope {scopeBindings = bb, scopeSubtypes = st}

joinAllScopes ::
       forall ts. HasInterpreter ts
    => [Scope ts]
    -> Scope ts
    -> Interpreter ts (Scope ts)
joinAllScopes [] s = return s
joinAllScopes (a:aa) s = do
    s' <- joinScopes a s
    joinAllScopes aa s'

type family EntryDoc (ts :: Type) :: Type

data Module ts = MkModule
    { moduleDoc :: Tree (EntryDoc ts)
    , moduleScope :: Scope ts
    }

type InterpretContext :: Type -> Type
data InterpretContext ts = MkInterpretContext
    { icSourcePos :: SourcePos
    , icVarIDState :: VarIDState
    , icScope :: Scope ts
    , icCurrentNamespace :: Namespace
    , icSpecialVals :: SpecialVals ts
    , icModulePath :: [ModuleName]
    , icLoadModule :: ModuleName -> Interpreter ts (Maybe (Module ts))
    }

type InterpretState :: Type -> Type
data InterpretState ts = MkInterpretState
    { isTypeID :: TypeID
    , isModules :: Map ModuleName (Module ts)
    }

emptyInterpretState :: InterpretState ts
emptyInterpretState = let
    isTypeID = zeroTypeID
    isModules = mempty
    in MkInterpretState {..}

type Interpreter :: Type -> Type -> Type
newtype Interpreter ts a = MkInterpreter
    { unInterpreter :: ReaderT (InterpretContext ts) (StateT (InterpretState ts) InterpretResult) a
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

mkErrorMessage :: ErrorType -> Interpreter ts ErrorMessage
mkErrorMessage err = do
    spos <- paramAsk sourcePosParam
    ntt <- getRenderFullName
    return $ MkErrorMessage spos ntt err mempty

instance MonadThrow ErrorType (Interpreter ts) where
    throw err = do
        em <- mkErrorMessage err
        throw em

instance MonadThrow PatternError (Interpreter ts) where
    throw err = throw $ PatternErrorError err

nameWitnessErrorType ::
       forall ts. AllConstraint ShowNamedText (TSNegWitness ts)
    => NonEmpty (Some (NameWitness VarID (TSNegShimWit ts)))
    -> ErrorType
nameWitnessErrorType ww = let
    nwToPair :: Some (NameWitness VarID (TSNegShimWit ts)) -> (FullNameRef, NamedText)
    nwToPair (MkSome (MkNameWitness varid (MkShimWit w _))) = let
        name =
            case varIdNameRef varid of
                FailureResult fnr -> fnr
                SuccessResult fn -> fullNameRef fn
        in withAllConstraint @Type @ShowNamedText w (name, showNamedText w)
    in ExpressionErrorError $ fmap nwToPair ww

instance (AllConstraint ShowNamedText (TSNegWitness ts), vw ~ TSNegShimWit ts) =>
             MonadThrow (NamedExpressionError VarID vw) (Interpreter ts) where
    throw (UndefinedBindingsError ww) = throw $ nameWitnessErrorType @ts ww

instance Semigroup a => Semigroup (Interpreter ts a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Interpreter ts a) where
    mappend = (<>)
    mempty = pure mempty

contextParam :: Param (Interpreter ts) (InterpretContext ts)
contextParam = MkParam (MkInterpreter ask) $ \a (MkInterpreter m) -> MkInterpreter $ with a m

sourcePosParam :: Param (Interpreter ts) SourcePos
sourcePosParam = lensMapParam (\bfb a -> fmap (\b -> a {icSourcePos = b}) $ bfb $ icSourcePos a) contextParam

varIDStateParam :: Param (Interpreter ts) VarIDState
varIDStateParam = lensMapParam (\bfb a -> fmap (\b -> a {icVarIDState = b}) $ bfb $ icVarIDState a) contextParam

scopeParam :: Param (Interpreter ts) (Scope ts)
scopeParam = lensMapParam (\bfb a -> fmap (\b -> a {icScope = b}) $ bfb $ icScope a) contextParam

bindingsParam :: Param (Interpreter ts) (NameMap ts)
bindingsParam = lensMapParam (\bfb a -> fmap (\b -> a {scopeBindings = b}) $ bfb $ scopeBindings a) scopeParam

currentNamespaceParam :: Param (Interpreter ts) Namespace
currentNamespaceParam =
    lensMapParam (\bfb a -> fmap (\b -> a {icCurrentNamespace = b}) $ bfb $ icCurrentNamespace a) contextParam

specialValsParam :: Param (Interpreter ts) (SpecialVals ts)
specialValsParam = lensMapParam (\bfb a -> fmap (\b -> a {icSpecialVals = b}) $ bfb $ icSpecialVals a) contextParam

modulePathParam :: Param (Interpreter ts) [ModuleName]
modulePathParam = lensMapParam (\bfb a -> fmap (\b -> a {icModulePath = b}) $ bfb $ icModulePath a) contextParam

loadModuleParam :: Param (Interpreter ts) (ModuleName -> Interpreter ts (Maybe (Module ts)))
loadModuleParam = lensMapParam (\bfb a -> fmap (\b -> a {icLoadModule = b}) $ bfb $ icLoadModule a) contextParam

interpretStateRef :: Ref (Interpreter ts) (InterpretState ts)
interpretStateRef = let
    ref = liftRef stateRef
    in MkRef (MkInterpreter $ refGet ref) $ \a -> MkInterpreter $ refPut ref a

typeIDRef :: Ref (Interpreter ts) TypeID
typeIDRef = lensMapRef (\bfb a -> fmap (\b -> a {isTypeID = b}) $ bfb $ isTypeID a) interpretStateRef

modulesRef :: Ref (Interpreter ts) (Map ModuleName (Module ts))
modulesRef = lensMapRef (\bfb a -> fmap (\b -> a {isModules = b}) $ bfb $ isModules a) interpretStateRef

runInterpreter ::
       SourcePos
    -> (ModuleName -> Interpreter ts (Maybe (Module ts)))
    -> SpecialVals ts
    -> Interpreter ts a
    -> InterpretResult a
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

getCurrentNamespace :: Interpreter ts Namespace
getCurrentNamespace = paramAsk currentNamespaceParam

withCurrentNamespace :: Namespace -> Interpreter ts --> Interpreter ts
withCurrentNamespace = paramWith currentNamespaceParam

namespacePriority :: Interpreter ts (NamespaceRef -> [Namespace])
namespacePriority = do
    curns <- getCurrentNamespace
    return $ namespaceConcatRefM $ toList $ namespaceAncestry curns

-- | For error messages and the like, doesn't need to be perfect.
getRenderFullName :: Interpreter ts (NamedText -> Text)
getRenderFullName = do
    curns <- getCurrentNamespace
    return $ runRelativeNamedText $ toList $ namespaceAncestry curns

nameMapLookupBindingInfo :: NameMap ts -> FullName -> Maybe (FullName, BindingInfo ts)
nameMapLookupBindingInfo (MkNameMap nspace) name = do
    bi <- lookup name nspace
    return (name, bi)

getBindingInfoMap :: Interpreter ts (FullNameRef -> Maybe (FullName, BindingInfo ts))
getBindingInfoMap = do
    nspace <- paramAsk bindingsParam
    nsp <- namespacePriority
    return $ \(MkFullNameRef name nsn) ->
        firstOf (nsp nsn) $ \ns -> nameMapLookupBindingInfo nspace $ MkFullName name ns

getBindingMap :: Interpreter ts (FullNameRef -> Maybe (InterpreterBinding ts))
getBindingMap = do
    bindmap <- getBindingInfoMap
    return $ \rname -> fmap (biValue . snd) $ bindmap rname

rnuToInterpreter :: FullNameRef -> Maybe a -> Interpreter ts a
rnuToInterpreter _ (Just a) = return a
rnuToInterpreter name Nothing = throw $ LookupNotDefinedError name

lookupBinding ::
       forall ts. HasInterpreter ts
    => FullNameRef
    -> Interpreter ts (InterpreterBinding ts)
lookupBinding name = do
    bindmap <- getBindingMap
    rnuToInterpreter name $ bindmap name

lookupDebugBindingInfo :: HasInterpreter ts => FullNameRef -> Interpreter ts (Maybe (FullName, String))
lookupDebugBindingInfo nameref = do
    bindmap <- getBindingInfoMap
    return $ fmap (\(name, b) -> (name, show $ biValue b)) $ bindmap nameref

lookupBoundType ::
       forall ts. HasInterpreter ts
    => FullNameRef
    -> Interpreter ts (InterpreterBoundType ts)
lookupBoundType name = do
    b <- lookupBinding name
    case b of
        TypeBinding t -> return t
        _ -> throw $ LookupNotTypeError name

lookupPatternConstructor ::
       forall ts. HasInterpreter ts
    => FullNameRef
    -> Interpreter ts (Either (TSExpressionPatternConstructor ts) (RecordConstructor ts))
lookupPatternConstructor name = do
    b <- lookupBinding name
    case b of
        ValueBinding _ (Just pc) -> return $ Left pc
        RecordConstructorBinding rc -> return $ Right rc
        _ -> throw $ LookupNotConstructorError name

lookupRecordConstructor ::
       forall ts. HasInterpreter ts
    => FullNameRef
    -> Interpreter ts (RecordConstructor ts)
lookupRecordConstructor name = do
    b <- lookupBinding name
    case b of
        RecordConstructorBinding rc -> return rc
        _ -> throw $ LookupNotRecordConstructorError name

lookupSpecialForm ::
       forall ts. HasInterpreter ts
    => FullNameRef
    -> Interpreter ts (SpecialForm ts (Interpreter ts))
lookupSpecialForm name = do
    b <- lookupBinding name
    case b of
        SpecialFormBinding sf -> return sf
        _ -> throw $ LookupNotSpecialFormError name

data BoundValue ts
    = ValueBoundValue (TSSealedExpression ts)
    | RecordBoundValue (RecordConstructor ts)

lookupBoundValue ::
       forall ts. HasInterpreter ts
    => FullNameRef
    -> Interpreter ts (BoundValue ts)
lookupBoundValue name = do
    b <- lookupBinding name
    case b of
        ValueBinding exp _ -> return $ ValueBoundValue exp
        RecordConstructorBinding rc -> return $ RecordBoundValue rc
        _ -> throw $ LookupNotConstructorError name

lookupMaybeValue ::
       forall ts. HasInterpreter ts
    => FullNameRef
    -> Interpreter ts (Maybe (TSSealedExpression ts))
lookupMaybeValue name = do
    mb <- getBindingMap
    return $
        case mb name of
            Just (ValueBinding exp _) -> Just exp
            _ -> Nothing

checkPureExpression ::
       forall ts. HasInterpreter ts
    => TSSealedExpression ts
    -> Interpreter ts ()
checkPureExpression expr = do
    _ <- tsEval @ts expr
    return ()

checkPureBinding :: HasInterpreter ts => InterpreterBinding ts -> Interpreter ts ()
checkPureBinding (ValueBinding expr _) = checkPureExpression expr
checkPureBinding _ = return ()

exportNames ::
       forall ts. HasInterpreter ts
    => [FullNameRef]
    -> Interpreter ts [(FullName, BindingInfo ts)]
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

exportNamespace :: [Namespace] -> Interpreter ts [(FullName, BindingInfo ts)]
exportNamespace cnss = do
    MkNameMap nspace <- paramAsk bindingsParam
    let
        toBI :: (FullName, BindingInfo ts) -> Bool
        toBI (MkFullName _ ns, _) = isJust $ choice $ fmap (\cns -> namespaceWithin cns ns) cnss
    return $ filter toBI $ mapToList nspace

exportScope ::
       forall ts. HasInterpreter ts
    => [Namespace]
    -> [FullNameRef]
    -> Interpreter ts ([FullName], Scope ts)
exportScope nsns names = do
    MkScope _ subtypes <- paramAsk scopeParam
    nsbindss <- exportNamespace nsns
    nbinds <- exportNames names
    let
        binds :: [(FullName, BindingInfo ts)]
        binds = nsbindss <> nbinds
    for_ binds $ \bi -> checkPureBinding $ biValue $ snd bi
    return $ (fmap fst binds, MkScope (bindingInfosToNameMap binds) subtypes)

type ScopeInterpreter ts = TransformT (Interpreter ts)

scopeRef :: Ref (ScopeInterpreter ts) (Scope ts)
scopeRef = transformParamRef scopeParam

bindingsRef :: Ref (ScopeInterpreter ts) (NameMap ts)
bindingsRef = transformParamRef bindingsParam

currentNamespaceRef :: Ref (ScopeInterpreter ts) Namespace
currentNamespaceRef = transformParamRef currentNamespaceParam

varIDStateRef :: Ref (ScopeInterpreter ts) VarIDState
varIDStateRef = transformParamRef varIDStateParam

scopeSourcePos :: SourcePos -> ScopeInterpreter ts ()
scopeSourcePos = refPut (transformParamRef sourcePosParam)

allocateVar ::
       forall ts. HasInterpreter ts
    => Maybe FullName
    -> ScopeInterpreter ts (FullName, VarID)
allocateVar mname = do
    vs <- refGet varIDStateRef
    let
        (vid, name) =
            case mname of
                Just name' -> (mkVarID vs name', name')
                Nothing -> mkUniqueVarID vs
        biOriginalName = name
        biDocumentation = fromString "variable"
        biValue = ValueBinding (tsVar @ts vid) Nothing
        insertScope = MkScope (bindingInfoToNameMap (name, MkBindingInfo {..})) mempty
    refPut varIDStateRef $ nextVarIDState vs
    refModifyM scopeRef $ \oldScope -> lift $ joinScopes insertScope oldScope
    return (name, vid)

getRestore :: Monad m => Ref m a -> m (m ())
getRestore r = do
    old <- refGet r
    return $ refPut r old

withCurrentNamespaceScope :: Namespace -> ScopeInterpreter ts (ScopeInterpreter ts ())
withCurrentNamespaceScope ns = do
    nrestore <- getRestore currentNamespaceRef
    refPut currentNamespaceRef ns
    return nrestore

usingNamespace :: Namespace -> Namespace -> (FullNameRef -> Bool) -> ScopeInterpreter ts ()
usingNamespace sourcens destns ff = refModify bindingsRef $ nameMapLookupNamespace sourcens destns ff

registerScope ::
       forall ts. HasInterpreter ts
    => Scope ts
    -> ScopeInterpreter ts ()
registerScope insertScope = refModifyM scopeRef $ \oldScope -> lift $ joinScopes insertScope oldScope

getCycle :: ModuleName -> [ModuleName] -> Maybe (NonEmpty ModuleName)
getCycle _ [] = Nothing
getCycle mn (n:nn)
    | mn == n = Just $ n :| nn
getCycle mn (_:nn) = getCycle mn nn

loadModuleInScope :: forall ts. ModuleName -> Interpreter ts (Maybe (Module ts))
loadModuleInScope mname =
    paramWith sourcePosParam (initialPos "<unknown>") $
    paramWith scopeParam emptyScope $
    paramLocal modulePathParam (\path -> path <> [mname]) $ do
        loadModule <- paramAsk loadModuleParam
        loadModule mname

getModule :: ModuleName -> Interpreter ts (Module ts)
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

registerBinding ::
       forall ts. HasInterpreter ts
    => FullName
    -> BindingInfo ts
    -> ScopeInterpreter ts ()
registerBinding name db = registerBindings $ singletonMap name db

getSubtypeScope :: SubtypeConversionEntry (InterpreterGroundType ts) -> Interpreter ts (Scope ts)
getSubtypeScope sce = do
    key <- liftIO newUnique
    return $ emptyScope {scopeSubtypes = singletonMap key sce}

registerBindings ::
       forall ts. HasInterpreter ts
    => [(FullName, BindingInfo ts)]
    -> ScopeInterpreter ts ()
registerBindings bb = do
    let newBindings = MkNameMap $ mapFromList bb
    refModify bindingsRef $ \oldBindings -> oldBindings <> newBindings

getSpecialVals :: Interpreter ts (SpecialVals ts)
getSpecialVals = paramAsk specialValsParam

registerLetBindings ::
       forall ts. HasInterpreter ts
    => [(FullName, RawMarkdown, TSSealedExpression ts)]
    -> ScopeInterpreter ts ()
registerLetBindings bb =
    registerBindings $ fmap (\(fname, doc, exp) -> (fname, MkBindingInfo fname doc $ ValueBinding exp Nothing)) bb

registerLetBinding ::
       forall ts. HasInterpreter ts
    => FullName
    -> RawMarkdown
    -> TSSealedExpression ts
    -> ScopeInterpreter ts ()
registerLetBinding name doc expr = registerLetBindings $ pure (name, doc, expr)

registerMatchBindings ::
       forall ts. HasInterpreter ts
    => TSMatch ts
    -> ScopeInterpreter ts ()
registerMatchBindings match = do
    let
        rbb =
            for (tsMatchBindings @ts match) $ \(wvar, expr) -> do
                vn <- varIdNameRef wvar
                return (vn, "lambda", expr)
    case rbb of
        SuccessResult bb -> registerLetBindings bb
        FailureResult fn -> lift $ throw $ KnownIssueError 0 $ "bad match var: " <> showNamedText fn

newTypeID :: Interpreter ts (Some TypeIDType)
newTypeID = do
    tid <- refGet typeIDRef
    refPut typeIDRef $ succTypeID tid
    return $ valueToSome tid

withNewTypeID :: (forall tid. TypeIDType tid -> Interpreter ts a) -> Interpreter ts a
withNewTypeID call = do
    stid <- newTypeID
    case stid of
        MkSome tid -> call tid

checkName :: FullName -> ScopeInterpreter ts ()
checkName name = do
    mnt <- lift getBindingMap
    case mnt $ fullNameRef name of
        Just _ -> lift $ throw $ DeclareTypeDuplicateError name
        Nothing -> return ()

registerBoundType ::
       forall ts. HasInterpreter ts
    => FullName
    -> RawMarkdown
    -> InterpreterBoundType ts
    -> ScopeInterpreter ts ()
registerBoundType name doc t = do
    checkName name
    registerBinding name $ MkBindingInfo name doc $ TypeBinding t

registerType ::
       forall ts dv t. HasInterpreter ts
    => FullName
    -> RawMarkdown
    -> InterpreterGroundType ts dv t
    -> ScopeInterpreter ts ()
registerType name doc t = do
    checkName name
    registerBoundType name doc $ MkSomeGroundType t

type ScopeFixBox ts = FixBox (ScopeInterpreter ts)

registerPatternConstructor ::
       forall ts. HasInterpreter ts
    => FullName
    -> RawMarkdown
    -> TSSealedExpression ts
    -> TSExpressionPatternConstructor ts
    -> ScopeInterpreter ts ()
registerPatternConstructor name doc exp pc = do
    checkName name
    registerBinding name $ MkBindingInfo name doc $ ValueBinding exp $ Just pc

registerRecord ::
       forall ts. HasInterpreter ts
    => FullName
    -> RawMarkdown
    -> RecordConstructor ts
    -> ScopeInterpreter ts ()
registerRecord name doc rc = do
    checkName name
    registerBinding name $ MkBindingInfo name doc $ RecordConstructorBinding rc

registerSubtypeConversion ::
       forall ts. HasInterpreter ts
    => SubtypeConversionEntry (InterpreterGroundType ts)
    -> ScopeInterpreter ts ()
registerSubtypeConversion sce = do
    newscope <- lift $ getSubtypeScope sce
    registerScope newscope

getSubtypeConversions :: Interpreter ts [SubtypeConversionEntry (InterpreterGroundType ts)]
getSubtypeConversions = fmap (fmap snd . mapToList . scopeSubtypes) $ paramAsk scopeParam
