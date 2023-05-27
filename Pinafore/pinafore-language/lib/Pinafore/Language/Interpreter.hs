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
    , ScopeInterpreter
    , sourcePosParam
    , scopeSourcePos
    , getBindingNamespace
    , withBindingNamespace
    , usingNamespace
    , getRenderFullName
    , throwWithName
    , SpecialVals(..)
    , getSpecialVals
    , NotUnique(..)
    , getBindingInfoMap
    , lookupMaybeValue
    , lookupBoundType
    , lookupPatternConstructor
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
    , FullName(..)
    , Signature(..)
    , RecordConstructor(..)
    , RecordPattern(..)
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

type InterpreterShimWit (ts :: Type) (polarity :: Polarity) = DolanShimWit (InterpreterGroundType ts) polarity

type InterpreterBoundType (ts :: Type) = SomeGroundType (InterpreterGroundType ts)

type HasInterpreter ts
     = ( ts ~ DolanTypeSystem (InterpreterGroundType ts)
       , IsDolanFunctionGroundType (InterpreterGroundType ts)
       , IsDolanSubtypeEntriesGroundType (InterpreterGroundType ts)
       , TSVarID ts ~ VarID
       , ExprShow (InterpreterBoundType ts)
       , Show (TSSealedExpression ts))

newtype SpecialVals (ts :: Type) = MkSpecialVals
    { specialEvaluate :: forall t. TSPosWitness ts t -> Text -> Action (Either Text t)
        -- ^ in Action because this can do things like import files
    }

data Signature (ts :: Type) (polarity :: Polarity) (t :: Type) =
    ValueSignature Name
                   (InterpreterType ts polarity t)

instance (HasInterpreter ts, Is PolarityType polarity) => HasVarMapping (Signature ts polarity) where
    getVarMapping (ValueSignature _ t) = getVarMapping t

data RecordConstructor (ts :: Type) =
    forall (t :: Type) (tt :: [Type]). MkRecordConstructor (ListType (Signature ts 'Positive) tt)
                                                           (InterpreterShimWit ts 'Positive t)
                                                           (ListVProduct tt -> t)

data RecordPattern (ts :: Type) =
    forall (t :: Type) (tt :: [Type]). MkRecordPattern (ListType (Signature ts 'Positive) tt)
                                                       (InterpreterShimWit ts 'Negative t)
                                                       (t -> Maybe (ListVProduct tt))

data InterpreterBinding (ts :: Type)
    = ValueBinding (TSSealedExpression ts)
                   (Maybe (TSExpressionPatternConstructor ts))
    | TypeBinding (InterpreterBoundType ts)
    | RecordConstructorBinding (RecordConstructor ts)
                               (RecordPattern ts)
    | SpecialFormBinding (SpecialForm ts (Interpreter ts))

instance HasInterpreter ts => Show (InterpreterBinding ts) where
    show (ValueBinding e Nothing) = "val: " <> show e
    show (ValueBinding e (Just _)) = "val+pat: " <> show e
    show (TypeBinding t) = "type: " <> unpack (toText $ exprShow t)
    show (RecordConstructorBinding _ _) = "recordpat"
    show (SpecialFormBinding _) = "special"

type DocInterpreterBinding ts = (RawMarkdown, InterpreterBinding ts)

newtype NameMap ts =
    MkNameMap (Map FullName (DocInterpreterBinding ts))

instance Semigroup (NameMap ts) where
    MkNameMap nsa <> MkNameMap nsb = let
        joinBindings _ bb = bb
        in MkNameMap $ unionWith joinBindings nsa nsb

instance Monoid (NameMap ts) where
    mempty = MkNameMap mempty

instance HasInterpreter ts => Show (NameMap ts) where
    show (MkNameMap m) = "{" <> intercalate "," (fmap (\(n, (_, b)) -> show n <> "=" <> show b) $ mapToList m) <> "}"

data BindingInfo ts = MkBindingInfo
    { biName :: FullName
    , biDocumentation :: RawMarkdown
    , biValue :: InterpreterBinding ts
    }

bindingInfoToNameMap :: BindingInfo ts -> NameMap ts
bindingInfoToNameMap MkBindingInfo {..} = MkNameMap $ singletonMap biName (biDocumentation, biValue)

bindingInfosToNameMap :: [BindingInfo ts] -> NameMap ts
bindingInfosToNameMap bis = mconcat $ fmap bindingInfoToNameMap bis

bindingInfosToScope :: [BindingInfo ts] -> Scope ts
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
checkEntryConsistency (MkSubtypeConversionEntry TrustMe _ _ _) _ = return ()
checkEntryConsistency (MkSubtypeConversionEntry Verify ta tb sconv) entries =
    case checkSubtypeConsistency (toList entries) (MkSomeGroundType ta) (MkSomeGroundType tb) of
        Nothing -> return ()
        Just (MkSubtypeConversionEntry _ eta etb esconv) ->
            if isNeutralSubtypeConversion sconv && isNeutralSubtypeConversion esconv
                then return ()
                else do
                    ntt <- getRenderFullName
                    throw $
                        InterpretSubtypeInconsistent
                            (ntt $ exprShow $ MkSomeGroundType eta)
                            (ntt $ exprShow $ MkSomeGroundType etb)

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
    , icBindingNamespace :: Namespace
    , icLookupNamespaces :: [(Namespace, FullNameRef -> Bool)]
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

instance MonadThrow ErrorType (Interpreter ts) where
    throw err = do
        spos <- paramAsk sourcePosParam
        throw $ MkErrorMessage spos err mempty

instance MonadThrow ExpressionError (Interpreter ts) where
    throw err = throw $ ExpressionErrorError err

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

bindingNamespaceParam :: Param (Interpreter ts) Namespace
bindingNamespaceParam =
    lensMapParam (\bfb a -> fmap (\b -> a {icBindingNamespace = b}) $ bfb $ icBindingNamespace a) contextParam

lookupNamespacesParam :: Param (Interpreter ts) [(Namespace, FullNameRef -> Bool)]
lookupNamespacesParam =
    lensMapParam (\bfb a -> fmap (\b -> a {icLookupNamespaces = b}) $ bfb $ icLookupNamespaces a) contextParam

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
    icBindingNamespace = RootNamespace
    icLookupNamespaces = [(RootNamespace, \_ -> True)]
    in evalStateT (runReaderT (unInterpreter qa) $ MkInterpretContext {..}) emptyInterpretState

data NotUnique a
    = MissingNotUnique
    | DuplicateNotUnique [a]

getUnique :: [a] -> (a -> Maybe b) -> Result (NotUnique a) b
getUnique aa f =
    case mapMaybe (\a -> fmap (\b -> (a, b)) $ f a) aa of
        [] -> FailureResult MissingNotUnique
        [(_, b)] -> SuccessResult b
        (a1, _):(a2, _):ab -> FailureResult $ DuplicateNotUnique $ a1 : a2 : fmap fst ab

getBindingNamespace :: Interpreter ts Namespace
getBindingNamespace = paramAsk bindingNamespaceParam

-- | For error messages and the like, doesn't need to be perfect.
getRenderFullName :: Interpreter ts (NamedText -> Text)
getRenderFullName = do
    curns <- getBindingNamespace
    return $ runRelativeNamedText $ toList $ namespaceAncestry curns

throwWithName :: ((NamedText -> Text) -> ErrorType) -> Interpreter ts a
throwWithName err = do
    ntt <- getRenderFullName
    throw $ err ntt

nameMapLookupBindingInfo :: NameMap ts -> FullName -> Maybe (BindingInfo ts)
nameMapLookupBindingInfo (MkNameMap nspace) name = do
    (biDocumentation, biValue) <- lookup name nspace
    let biName = name
    return MkBindingInfo {..}

getBindingInfoMap :: Interpreter ts (FullNameRef -> Result (NotUnique FullName) (BindingInfo ts))
getBindingInfoMap = do
    nmap <- paramAsk bindingsParam
    nspaces <- paramAsk lookupNamespacesParam
    return $ \nref -> let
        goodName :: (Namespace, FullNameRef -> Bool) -> Maybe FullName
        goodName (nspace, pick) =
            if pick nref
                then Just $ namespaceConcatFullName nspace nref
                else Nothing
        fnames = nub $ mapMaybe goodName nspaces
        in getUnique fnames $ nameMapLookupBindingInfo nmap

getBindingMap :: Interpreter ts (FullNameRef -> Result (NotUnique FullName) (InterpreterBinding ts))
getBindingMap = do
    bindmap <- getBindingInfoMap
    return $ \rname -> fmap biValue $ bindmap rname

rnuToInterpreter :: FullNameRef -> Result (NotUnique FullName) a -> Interpreter ts a
rnuToInterpreter _ (SuccessResult a) = return a
rnuToInterpreter name (FailureResult MissingNotUnique) = throw $ LookupNotDefinedError name
rnuToInterpreter name (FailureResult (DuplicateNotUnique nn)) = throw $ LookupAmbiguousError name nn

lookupBinding ::
       forall ts. HasInterpreter ts
    => FullNameRef
    -> Interpreter ts (InterpreterBinding ts)
lookupBinding name = do
    bindmap <- getBindingMap
    rnuToInterpreter name $ bindmap name

lookupDebugBindingInfo :: HasInterpreter ts => FullNameRef -> Interpreter ts (FullName, String)
lookupDebugBindingInfo name = do
    bindmap <- getBindingInfoMap
    b <- rnuToInterpreter name $ bindmap name
    return $ (biName b, show $ biValue b)

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
    -> Interpreter ts (Either (TSExpressionPatternConstructor ts) (RecordPattern ts))
lookupPatternConstructor name = do
    b <- lookupBinding name
    case b of
        ValueBinding _ (Just pc) -> return $ Left pc
        RecordConstructorBinding _ rp -> return $ Right rp
        _ -> throw $ LookupNotConstructorError name

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
        RecordConstructorBinding rc _ -> return $ RecordBoundValue rc
        _ -> throw $ LookupNotConstructorError name

lookupMaybeValue ::
       forall ts. HasInterpreter ts
    => FullNameRef
    -> Interpreter ts (Maybe (TSSealedExpression ts))
lookupMaybeValue name = do
    mb <- getBindingMap
    return $
        case mb name of
            SuccessResult (ValueBinding exp _) -> Just exp
            _ -> Nothing

checkPureExpression ::
       forall ts. (Show (TSVarID ts), AllConstraint Show (TSNegWitness ts))
    => TSSealedExpression ts
    -> Interpreter ts ()
checkPureExpression expr = do
    _ <- tsEval @ts expr
    return ()

checkPureBinding ::
       (Show (TSVarID ts), AllConstraint Show (TSNegWitness ts)) => InterpreterBinding ts -> Interpreter ts ()
checkPureBinding (ValueBinding expr _) = checkPureExpression expr
checkPureBinding _ = return ()

exportNames ::
       forall ts. (Show (TSVarID ts), AllConstraint Show (TSNegWitness ts))
    => [FullNameRef]
    -> Interpreter ts [BindingInfo ts]
exportNames names = do
    bindmap <- getBindingInfoMap
    let
        (badnamesl, bis) =
            partitionEithers $
            fmap
                (\name ->
                     case bindmap name of
                         SuccessResult bi -> Right bi
                         FailureResult _ -> Left name)
                names
    case nonEmpty badnamesl of
        Just badnames -> throw $ LookupNamesUnknownError badnames
        Nothing -> return bis

exportNamespace :: [Namespace] -> Interpreter ts [BindingInfo ts]
exportNamespace cnss = do
    MkNameMap nspace <- paramAsk bindingsParam
    let
        toBI :: (FullName, (RawMarkdown, InterpreterBinding ts)) -> Maybe (BindingInfo ts)
        toBI (biName@(MkFullName _ ns), (biDocumentation, biValue)) = do
            _ <- choice $ fmap (\cns -> namespaceWithin cns ns) cnss
            return MkBindingInfo {..}
    return $ mapMaybe toBI $ mapToList nspace

exportScope ::
       forall ts. (Show (TSVarID ts), AllConstraint Show (TSNegWitness ts))
    => [Namespace]
    -> [FullNameRef]
    -> Interpreter ts ([FullName], Scope ts)
exportScope nsns names = do
    MkScope _ subtypes <- paramAsk scopeParam
    nsbindss <- exportNamespace nsns
    nbinds <- exportNames names
    let binds = nsbindss <> nbinds
    for_ binds $ \bi -> checkPureBinding $ biValue bi
    return $ (fmap biName binds, MkScope (bindingInfosToNameMap binds) subtypes)

type ScopeInterpreter ts = TransformT (Interpreter ts)

scopeRef :: Ref (ScopeInterpreter ts) (Scope ts)
scopeRef = transformParamRef scopeParam

bindingsRef :: Ref (ScopeInterpreter ts) (NameMap ts)
bindingsRef = transformParamRef bindingsParam

bindingNamespaceRef :: Ref (ScopeInterpreter ts) Namespace
bindingNamespaceRef = transformParamRef bindingNamespaceParam

lookupNamespacesRef :: Ref (ScopeInterpreter ts) [(Namespace, FullNameRef -> Bool)]
lookupNamespacesRef = transformParamRef lookupNamespacesParam

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
        (vid, biName) =
            case mname of
                Just name -> (mkVarID vs name, name)
                Nothing -> mkUniqueVarID vs
        biDocumentation = fromString "variable"
        biValue = ValueBinding (tsVar @ts vid) Nothing
        insertScope = MkScope (bindingInfoToNameMap MkBindingInfo {..}) mempty
    refPut varIDStateRef $ nextVarIDState vs
    refModifyM scopeRef $ \oldScope -> lift $ joinScopes insertScope oldScope
    return (biName, vid)

getRestore :: Monad m => Ref m a -> m (m ())
getRestore r = do
    old <- refGet r
    return $ refPut r old

withBindingNamespace :: Namespace -> ScopeInterpreter ts (ScopeInterpreter ts ())
withBindingNamespace ns = do
    nrestore <- getRestore $ bindingNamespaceRef <***> lookupNamespacesRef
    refPut bindingNamespaceRef ns
    usingNamespace ns $ \_ -> True
    return nrestore

usingNamespace :: Namespace -> (FullNameRef -> Bool) -> ScopeInterpreter ts ()
usingNamespace ns pick = refModify lookupNamespacesRef $ \nss -> (ns, pick) : nss

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

registerBinding :: FullName -> DocInterpreterBinding ts -> ScopeInterpreter ts ()
registerBinding name db = registerBindings $ singletonMap name db

getSubtypeScope :: SubtypeConversionEntry (InterpreterGroundType ts) -> Interpreter ts (Scope ts)
getSubtypeScope sce = do
    key <- liftIO newUnique
    return $ emptyScope {scopeSubtypes = singletonMap key sce}

registerBindings :: [(FullName, DocInterpreterBinding ts)] -> ScopeInterpreter ts ()
registerBindings bb = do
    let newBindings = MkNameMap $ mapFromList bb
    refModify bindingsRef $ \oldBindings -> oldBindings <> newBindings

getSpecialVals :: Interpreter ts (SpecialVals ts)
getSpecialVals = paramAsk specialValsParam

registerLetBindings ::
       forall ts. HasInterpreter ts
    => [(FullName, RawMarkdown, TSSealedExpression ts)]
    -> ScopeInterpreter ts ()
registerLetBindings bb = registerBindings $ fmap (\(nref, doc, exp) -> (nref, (doc, ValueBinding exp Nothing))) bb

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
        FailureResult fn -> lift $ throw $ KnownIssueError 0 $ "bad match var: " <> toText fn

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
        FailureResult MissingNotUnique -> return ()
        _ -> lift $ throw $ DeclareTypeDuplicateError name

registerBoundType :: FullName -> RawMarkdown -> InterpreterBoundType ts -> ScopeInterpreter ts ()
registerBoundType name doc t = do
    checkName name
    registerBinding name (doc, TypeBinding t)

registerType :: FullName -> RawMarkdown -> InterpreterGroundType ts dv t -> ScopeInterpreter ts ()
registerType name doc t = do
    checkName name
    registerBoundType name doc $ MkSomeGroundType t

type ScopeFixBox ts = FixBox (ScopeInterpreter ts)

registerPatternConstructor ::
       FullName -> RawMarkdown -> TSSealedExpression ts -> TSExpressionPatternConstructor ts -> ScopeInterpreter ts ()
registerPatternConstructor name doc exp pc = do
    checkName name
    registerBinding name $ (doc, ValueBinding exp $ Just pc)

registerRecord :: FullName -> RawMarkdown -> RecordConstructor ts -> RecordPattern ts -> ScopeInterpreter ts ()
registerRecord name doc rc rp = do
    checkName name
    registerBinding name $ (doc, RecordConstructorBinding rc rp)

registerSubtypeConversion ::
       forall ts. HasInterpreter ts
    => SubtypeConversionEntry (InterpreterGroundType ts)
    -> ScopeInterpreter ts ()
registerSubtypeConversion sce = do
    newscope <- lift $ getSubtypeScope sce
    registerScope newscope

getSubtypeConversions :: Interpreter ts [SubtypeConversionEntry (InterpreterGroundType ts)]
getSubtypeConversions = fmap (fmap snd . mapToList . scopeSubtypes) $ paramAsk scopeParam
