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
    , withNamespace
    , usingNamespace
    , SpecialVals(..)
    , getSpecialVals
    , lookupLetBinding
    , registerLetBindings
    , registerLetBinding
    , getSubtypeScope
    , lookupSpecialForm
    , lookupBoundType
    , newTypeID
    , withNewTypeID
    , registerBoundType
    , registerType
    , ScopeFixBox
    , FullName(..)
    , getNameResolver
    , BindingInfo(..)
    , getBindingMap
    , bindingInfosToScope
    , InterpreterBinding(..)
    , lookupPatternConstructor
    , registerPatternConstructor
    , registerSubtypeConversion
    , getSubtypeConversions
    ) where

import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Name
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type.Identified
import Pinafore.Language.VarID
import Pinafore.Markdown
import Shapes
import Text.Parsec.Pos (SourcePos, initialPos)

type family InterpreterGroundType (ts :: Type) :: GroundTypeKind

type instance forall (gt :: GroundTypeKind). InterpreterGroundType (DolanTypeSystem gt) = gt

type family InterpreterFamilyType (ts :: Type) :: forall k. k -> Type

type InterpreterBoundType (ts :: Type) = SomeGroundType (InterpreterGroundType ts)

newtype SpecialVals (ts :: Type) = MkSpecialVals
    { specialEvaluate :: forall t. TSPosWitness ts t -> Text -> Action (Either Text t)
        -- ^ in Action because this can do things like import files
    }

data InterpreterBinding (ts :: Type)
    = LambdaBinding VarID
    | ValueBinding (TSSealedExpression ts)
                   (Maybe (TSExpressionPatternConstructor ts))
    | TypeBinding (InterpreterBoundType ts)
    | SpecialFormBinding (SpecialForm ts (Interpreter ts))

instance Show (InterpreterBinding ts) where
    show (LambdaBinding _) = "fn"
    show (ValueBinding _ Nothing) = "val"
    show (ValueBinding _ (Just _)) = "val+pat"
    show (TypeBinding _) = "type"
    show (SpecialFormBinding _) = "special"

type DocInterpreterBinding ts = (Markdown, InterpreterBinding ts)

newtype NameMap ts =
    MkNameMap (Map FullName (DocInterpreterBinding ts))

instance Semigroup (NameMap ts) where
    MkNameMap nsa <> MkNameMap nsb = let
        joinBindings _ bb = bb
        in MkNameMap $ unionWith joinBindings nsa nsb

instance Monoid (NameMap ts) where
    mempty = MkNameMap mempty

instance Show (NameMap ts) where
    show (MkNameMap m) = "{" <> intercalate "," (fmap (\(n, (_, b)) -> show n <> "=" <> show b) $ mapToList m) <> "}"

data BindingInfo ts = MkBindingInfo
    { biName :: FullName
    , biDocumentation :: Markdown
    , biValue :: InterpreterBinding ts
    }

bindingInfoToNameMap :: BindingInfo ts -> NameMap ts
bindingInfoToNameMap MkBindingInfo {..} = MkNameMap $ singletonMap biName (biDocumentation, biValue)

bindingInfosToNameMap :: [BindingInfo ts] -> NameMap ts
bindingInfosToNameMap bis = mconcat $ fmap bindingInfoToNameMap bis

bindingInfosToScope :: [BindingInfo ts] -> Scope ts
bindingInfosToScope bis = emptyScope {scopeBindings = bindingInfosToNameMap bis}

nameMapLookupBindingInfo :: NameMap ts -> FullName -> Maybe (BindingInfo ts)
nameMapLookupBindingInfo (MkNameMap nspace) name = do
    (biDocumentation, biValue) <- lookup name nspace
    let biName = name
    return MkBindingInfo {..}

data Scope (ts :: Type) = MkScope
    { scopeBindings :: NameMap ts
    , scopeSubtypes :: HashMap Unique (SubtypeConversionEntry (InterpreterGroundType ts))
    }

emptyScope :: Scope ts
emptyScope = MkScope mempty mempty

type HasInterpreter ts
     = (IsDolanSubtypeEntriesGroundType (InterpreterGroundType ts), ExprShow (InterpreterBoundType ts))

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
                else throw $
                     InterpretSubtypeInconsistent (exprShow $ MkSomeGroundType eta) (exprShow $ MkSomeGroundType etb)

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
    { moduleDoc :: DocTree (EntryDoc ts)
    , moduleScope :: Scope ts
    }

type InterpretContext :: Type -> Type
data InterpretContext ts = MkInterpretContext
    { icSourcePos :: SourcePos
    , icVarIDState :: VarIDState
    , icScope :: Scope ts
    , icNamespace :: Namespace
    , icUsingNamespaces :: [Namespace]
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

namespaceParam :: Param (Interpreter ts) Namespace
namespaceParam = lensMapParam (\bfb a -> fmap (\b -> a {icNamespace = b}) $ bfb $ icNamespace a) contextParam

usingNamespacesParam :: Param (Interpreter ts) [Namespace]
usingNamespacesParam =
    lensMapParam (\bfb a -> fmap (\b -> a {icUsingNamespaces = b}) $ bfb $ icUsingNamespaces a) contextParam

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
    icNamespace = RootNamespace
    icUsingNamespaces = []
    in evalStateT (runReaderT (unInterpreter qa) $ MkInterpretContext {..}) emptyInterpretState

firstOf :: [a] -> (a -> Maybe b) -> Maybe b
firstOf [] _ = Nothing
firstOf (a:aa) amb =
    case amb a of
        Just b -> Just b
        Nothing -> firstOf aa amb

nsAndParents :: Namespace -> [Namespace]
nsAndParents a@(MkNamespace nn) =
    a :
    case nonEmpty nn of
        Nothing -> []
        Just na -> nsAndParents $ MkNamespace $ init na

namespacePriority :: Interpreter ts (NamespaceRef -> [Namespace])
namespacePriority = do
    curns <- paramAsk namespaceParam
    usingnss <- paramAsk usingNamespacesParam
    return $ \case
        AbsoluteNamespaceRef ann -> [ann]
        RelativeNamespaceRef rnn -> fmap (\ns -> namespaceConcat ns rnn) $ nsAndParents curns <> usingnss

namespaceCurrent :: NamespaceRef -> Interpreter ts Namespace
namespaceCurrent (AbsoluteNamespaceRef ann) = return ann
namespaceCurrent (RelativeNamespaceRef rnn) = do
    curns <- paramAsk namespaceParam
    return $ namespaceConcat curns rnn

getBindingMap :: Interpreter ts (FullNameRef -> Maybe (BindingInfo ts))
getBindingMap = do
    (scopeBindings -> nspace) <- paramAsk scopeParam
    nsp <- namespacePriority
    return $ \(MkFullNameRef nsn name) ->
        firstOf (nsp nsn) $ \ns -> nameMapLookupBindingInfo nspace $ MkFullName ns name

lookupBinding :: FullNameRef -> Interpreter ts (Maybe (InterpreterBinding ts))
lookupBinding rname = do
    bindmap <- getBindingMap
    return $ fmap biValue $ bindmap rname

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
    bindmap <- getBindingMap
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
        Just badnames -> throw $ LookupNamesUnknownError badnames
        Nothing -> return bis

exportNamespace :: [NamespaceRef] -> Interpreter ts [BindingInfo ts]
exportNamespace nsns = do
    cnss <- for nsns namespaceCurrent
    (scopeBindings -> MkNameMap nspace) <- paramAsk scopeParam
    let
        toBI :: (FullName, (Markdown, InterpreterBinding ts)) -> Maybe (BindingInfo ts)
        toBI (biName@(MkFullName ns _), (biDocumentation, biValue)) = do
            _ <- choice $ fmap (\cns -> namespaceStartsWith cns ns) cnss
            return MkBindingInfo {..}
    return $ mapMaybe toBI $ mapToList nspace

exportScope ::
       forall ts. (Show (TSVarID ts), AllConstraint Show (TSNegWitness ts))
    => [NamespaceRef]
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

namespaceRef :: Ref (ScopeInterpreter ts) Namespace
namespaceRef = transformParamRef namespaceParam

usingNamespacesRef :: Ref (ScopeInterpreter ts) [Namespace]
usingNamespacesRef = transformParamRef usingNamespacesParam

varIDStateRef :: Ref (ScopeInterpreter ts) VarIDState
varIDStateRef = transformParamRef varIDStateParam

getNameResolver :: ScopeInterpreter ts (FullNameRef -> FullName)
getNameResolver = do
    ns <- refGet namespaceRef
    return $ fullNameRefInNamespace ns

allocateVar ::
       forall ts. HasInterpreter ts
    => Maybe FullNameRef
    -> ScopeInterpreter ts (FullName, VarID)
allocateVar mnameref = do
    vs <- refGet varIDStateRef
    nsp <- getNameResolver
    let
        (vid, biName) =
            case mnameref of
                Just nameref -> let
                    name = nsp nameref
                    in (mkVarID vs name, name)
                Nothing -> mkUniqueVarID vs
        biDocumentation = plainMarkdown "variable"
        biValue = LambdaBinding vid
        insertScope = MkScope (bindingInfoToNameMap MkBindingInfo {..}) mempty
    refPut varIDStateRef $ nextVarIDState vs
    refModifyM scopeRef $ \oldScope -> lift $ joinScopes insertScope oldScope
    return (biName, vid)

getRestore :: Monad m => Ref m a -> m (m ())
getRestore r = do
    old <- refGet r
    return $ refPut r old

withNamespace :: NamespaceRef -> ScopeInterpreter ts (ScopeInterpreter ts ())
withNamespace nsn = do
    ns <- lift $ namespaceCurrent nsn
    nrestore <- getRestore namespaceRef
    urestore <- getRestore usingNamespacesRef
    refPut namespaceRef ns
    return $ urestore >> nrestore

usingNamespace :: NamespaceRef -> ScopeInterpreter ts ()
usingNamespace nsn = do
    ns <- lift $ namespaceCurrent nsn
    refModify usingNamespacesRef $ \uns -> ns : uns

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

registerBinding :: FullNameRef -> DocInterpreterBinding ts -> ScopeInterpreter ts ()
registerBinding name db = registerBindings $ singletonMap name db

getSubtypeScope :: SubtypeConversionEntry (InterpreterGroundType ts) -> Interpreter ts (Scope ts)
getSubtypeScope sce = do
    key <- liftIO newUnique
    return $ emptyScope {scopeSubtypes = singletonMap key sce}

registerBindings :: [(FullNameRef, DocInterpreterBinding ts)] -> ScopeInterpreter ts ()
registerBindings bb = do
    nsp <- getNameResolver
    let newBindings = MkNameMap $ mapFromList $ fmap (\(n, b) -> (nsp n, b)) bb
    refModify scopeRef $ \oldScope -> oldScope {scopeBindings = scopeBindings oldScope <> newBindings}

getSpecialVals :: Interpreter ts (SpecialVals ts)
getSpecialVals = paramAsk specialValsParam

lookupLetBinding :: FullNameRef -> Interpreter ts (Maybe (Either VarID (TSSealedExpression ts)))
lookupLetBinding name = do
    mb <- lookupBinding name
    case mb of
        Just (ValueBinding exp _) -> return $ Just $ Right exp
        Just (LambdaBinding v) -> return $ Just $ Left v
        _ -> return Nothing

registerLetBindings ::
       forall ts. HasInterpreter ts
    => [(FullNameRef, Markdown, TSSealedExpression ts)]
    -> ScopeInterpreter ts ()
registerLetBindings bb = registerBindings $ fmap (\(name, doc, exp) -> (name, (doc, ValueBinding exp Nothing))) bb

registerLetBinding ::
       forall ts. HasInterpreter ts
    => FullNameRef
    -> Markdown
    -> TSSealedExpression ts
    -> ScopeInterpreter ts ()
registerLetBinding name doc expr = registerLetBindings $ pure (name, doc, expr)

lookupSpecialForm :: FullNameRef -> Interpreter ts (SpecialForm ts (Interpreter ts))
lookupSpecialForm name = do
    mb <- lookupBinding name
    case mb of
        Just (SpecialFormBinding sf) -> return sf
        _ -> throw $ LookupSpecialFormUnknownError name

lookupBoundTypeM :: FullNameRef -> Interpreter ts (Maybe (InterpreterBoundType ts))
lookupBoundTypeM name = do
    mb <- lookupBinding name
    return $
        case mb of
            Just (TypeBinding t) -> Just t
            _ -> Nothing

lookupBoundType :: FullNameRef -> Interpreter ts (InterpreterBoundType ts)
lookupBoundType name = do
    mnt <- lookupBoundTypeM name
    case mnt of
        Just nt -> return nt
        Nothing -> throw $ LookupTypeUnknownError name

lookupPatternConstructorM :: FullNameRef -> Interpreter ts (Maybe (TSExpressionPatternConstructor ts))
lookupPatternConstructorM name = do
    mb <- lookupBinding name
    return $
        case mb of
            Just (ValueBinding _ (Just pc)) -> Just pc
            _ -> Nothing

lookupPatternConstructor :: FullNameRef -> Interpreter ts (TSExpressionPatternConstructor ts)
lookupPatternConstructor name = do
    ma <- lookupPatternConstructorM name
    case ma of
        Just a -> return a
        Nothing -> throw $ LookupConstructorUnknownError name

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

registerBoundType :: FullNameRef -> Markdown -> InterpreterBoundType ts -> ScopeInterpreter ts ()
registerBoundType name doc t = do
    mnt <- lift $ lookupBinding name
    case mnt of
        Just _ -> lift $ throw $ DeclareTypeDuplicateError name
        Nothing -> registerBinding name (doc, TypeBinding t)

registerType :: FullNameRef -> Markdown -> InterpreterGroundType ts dv t -> ScopeInterpreter ts ()
registerType name doc t = registerBoundType name doc $ MkSomeGroundType t

type ScopeFixBox ts = FixBox (ScopeInterpreter ts)

registerPatternConstructor ::
       FullNameRef -> Markdown -> TSSealedExpression ts -> TSExpressionPatternConstructor ts -> ScopeInterpreter ts ()
registerPatternConstructor name doc exp pc = do
    ma <- lift $ lookupBinding name
    case ma of
        Just _ -> lift $ throw $ DeclareConstructorDuplicateError name
        Nothing -> registerBinding name $ (doc, ValueBinding exp $ Just pc)

registerSubtypeConversion ::
       forall ts. HasInterpreter ts
    => SubtypeConversionEntry (InterpreterGroundType ts)
    -> ScopeInterpreter ts ()
registerSubtypeConversion sce = do
    newscope <- lift $ getSubtypeScope sce
    registerScope newscope

getSubtypeConversions :: Interpreter ts [SubtypeConversionEntry (InterpreterGroundType ts)]
getSubtypeConversions = fmap (fmap snd . mapToList . scopeSubtypes) $ paramAsk scopeParam
