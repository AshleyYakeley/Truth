module Pinafore.Language.Interpreter
    ( IsInterpreterGroundType(..)
    , PatternWitness(..)
    , SomeGroundType
    , runInterpreter
    , allocateVar
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
    , getCurrentNamespace
    , withNamespace
    , usingNamespace
    , getRenderFullName
    , throwWithName
    , SpecialVals(..)
    , getSpecialVals
    , BoundValue(..)
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
    , Signature(..)
    , RecordConstructor(..)
    , RecordPattern(..)
    , BindingInfo(..)
    , getBindingMap
    , bindingInfosToScope
    , InterpreterBinding(..)
    , registerMatchBindings
    , lookupPatternConstructor
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
import Pinafore.Markdown
import Shapes
import Text.Parsec.Pos (SourcePos, initialPos)

class ( IsDolanFunctionGroundType ground
      , IsDolanSubtypeEntriesGroundType ground
      , DolanPatternWitness ground ~ PatternWitness ground
      , DolanVarID ground ~ VarID
      , ExprShow (SomeGroundType ground)
      , Show (TSSealedExpression (DolanTypeSystem ground))
      ) => IsInterpreterGroundType (ground :: GroundTypeKind) where
    type EntryDoc ground :: Type
    createGroundType :: FullName -> Interpreter ground (SomeGroundType ground)

data PatternWitness (ground :: GroundTypeKind) t
    = ValuePatternWitness VarID
                          (DolanShimWit ground 'Positive t)
    | TypePatternWitness FullName

instance forall (ground :: GroundTypeKind) poswit. (poswit ~ DolanShimWit ground 'Positive) =>
             IsPatternWitness poswit (PatternWitness ground) where
    traversePatternWitness (MkEndoM f) =
        MkEndoM $ \case
            ValuePatternWitness vid w -> fmap (ValuePatternWitness vid) $ f w
            TypePatternWitness fn -> pure $ TypePatternWitness fn

newtype SpecialVals (ground :: GroundTypeKind) = MkSpecialVals
    { specialEvaluate :: forall t. DolanType ground 'Positive t -> Text -> Action (Either Text t)
        -- ^ in Action because this can do things like import files
    }

data Signature (ground :: GroundTypeKind) (polarity :: Polarity) (t :: Type)
    = TypeSignature Name
    | ValueSignature Name
                     (DolanType ground polarity t)

instance forall (ground :: GroundTypeKind) polarity. (IsInterpreterGroundType ground, Is PolarityType polarity) =>
             HasVarMapping (Signature ground polarity) where
    getVarMapping (TypeSignature _) = mempty
    getVarMapping (ValueSignature _ t) = getVarMapping t

data RecordConstructor (ground :: GroundTypeKind) =
    forall (t :: Type) (tt :: [Type]). MkRecordConstructor (ListType (Signature ground 'Positive) tt)
                                                           (DolanShimWit ground 'Positive t)
                                                           (ListVProduct tt -> t)

data RecordPattern (ground :: GroundTypeKind) =
    forall (t :: Type) (tt :: [Type]). MkRecordPattern (ListType (Signature ground 'Positive) tt)
                                                       (DolanShimWit ground 'Negative t)
                                                       (t -> Maybe (ListVProduct tt))

data InterpreterBinding (ground :: GroundTypeKind)
    = ValueBinding (TSSealedExpression (DolanTypeSystem ground))
                   (Maybe (TSExpressionPatternConstructor (DolanTypeSystem ground)))
    | TypeBinding (SomeGroundType ground)
    | RecordConstructorBinding (RecordConstructor ground)
                               (RecordPattern ground)
    | SpecialFormBinding (SpecialForm (DolanTypeSystem ground) (Interpreter ground))

instance forall (ground :: GroundTypeKind). IsInterpreterGroundType ground => Show (InterpreterBinding ground) where
    show (ValueBinding e Nothing) = "val: " <> show e
    show (ValueBinding e (Just _)) = "val+pat: " <> show e
    show (TypeBinding t) = "type: " <> unpack (toText $ exprShow t)
    show (RecordConstructorBinding _ _) = "recordpat"
    show (SpecialFormBinding _) = "special"

type DocInterpreterBinding (ground :: GroundTypeKind) = (RawMarkdown, InterpreterBinding ground)

newtype NameMap (ground :: GroundTypeKind) =
    MkNameMap (Map FullName (DocInterpreterBinding ground))

instance forall (ground :: GroundTypeKind). Semigroup (NameMap ground) where
    MkNameMap nsa <> MkNameMap nsb = let
        joinBindings _ bb = bb
        in MkNameMap $ unionWith joinBindings nsa nsb

instance forall (ground :: GroundTypeKind). Monoid (NameMap ground) where
    mempty = MkNameMap mempty

instance forall (ground :: GroundTypeKind). IsInterpreterGroundType ground => Show (NameMap ground) where
    show (MkNameMap m) = "{" <> intercalate "," (fmap (\(n, (_, b)) -> show n <> "=" <> show b) $ mapToList m) <> "}"

nameMapLookupNamespace ::
       forall (ground :: GroundTypeKind).
       Namespace
    -> Namespace
    -> (FullNameRef -> Bool)
    -> NameMap ground
    -> NameMap ground
nameMapLookupNamespace sourcens destns ff (MkNameMap nm) = let
    matchNS :: forall a. (FullName, a) -> Maybe (FullName, a)
    matchNS (fn, a) = do
        fnr <- namespaceWithinFullNameRef sourcens fn
        altIf $ ff fnr
        return (namespaceConcatFullName destns fnr, a)
    newEntries = mapMaybe matchNS $ mapToList nm
    in MkNameMap $ mapFromList newEntries <> nm

data BindingInfo (ground :: GroundTypeKind) = MkBindingInfo
    { biName :: FullName
    , biDocumentation :: RawMarkdown
    , biValue :: InterpreterBinding ground
    }

bindingInfoToNameMap :: forall (ground :: GroundTypeKind). BindingInfo ground -> NameMap ground
bindingInfoToNameMap MkBindingInfo {..} = MkNameMap $ singletonMap biName (biDocumentation, biValue)

bindingInfosToNameMap :: forall (ground :: GroundTypeKind). [BindingInfo ground] -> NameMap ground
bindingInfosToNameMap bis = mconcat $ fmap bindingInfoToNameMap bis

bindingInfosToScope :: forall (ground :: GroundTypeKind). [BindingInfo ground] -> Scope ground
bindingInfosToScope bis = emptyScope {scopeBindings = bindingInfosToNameMap bis}

nameMapLookupBindingInfo :: forall (ground :: GroundTypeKind). NameMap ground -> FullName -> Maybe (BindingInfo ground)
nameMapLookupBindingInfo (MkNameMap nspace) name = do
    (biDocumentation, biValue) <- lookup name nspace
    let biName = name
    return MkBindingInfo {..}

data Scope (ground :: GroundTypeKind) = MkScope
    { scopeBindings :: NameMap ground
    , scopeSubtypes :: HashMap Unique (SubtypeConversionEntry ground)
    }

emptyScope :: forall (ground :: GroundTypeKind). Scope ground
emptyScope = MkScope mempty mempty

checkEntryConsistency ::
       forall (ground :: GroundTypeKind). IsInterpreterGroundType ground
    => SubtypeConversionEntry ground
    -> HashMap Unique (SubtypeConversionEntry ground)
    -> Interpreter ground ()
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
       forall (ground :: GroundTypeKind). IsInterpreterGroundType ground
    => (Unique, SubtypeConversionEntry ground)
    -> HashMap Unique (SubtypeConversionEntry ground)
    -> Interpreter ground (HashMap Unique (SubtypeConversionEntry ground))
addSCEntry (key, _) entries
    | member key entries = return entries
addSCEntry (key, entry) entries = do
    checkEntryConsistency entry entries
    return $ insertMap key entry entries

addSCEntries ::
       forall (ground :: GroundTypeKind). IsInterpreterGroundType ground
    => [(Unique, SubtypeConversionEntry ground)]
    -> HashMap Unique (SubtypeConversionEntry ground)
    -> Interpreter ground (HashMap Unique (SubtypeConversionEntry ground))
addSCEntries [] entries = return entries
addSCEntries (a:aa) entries = do
    entries' <- addSCEntry a entries
    addSCEntries aa entries'

joinScopes ::
       forall (ground :: GroundTypeKind). IsInterpreterGroundType ground
    => Scope ground
    -> Scope ground
    -> Interpreter ground (Scope ground)
joinScopes a b = do
    let
        bb = scopeBindings b <> scopeBindings a
        alist = mapToList $ scopeSubtypes a
    st <- addSCEntries alist $ scopeSubtypes b
    return MkScope {scopeBindings = bb, scopeSubtypes = st}

joinAllScopes ::
       forall (ground :: GroundTypeKind). IsInterpreterGroundType ground
    => [Scope ground]
    -> Scope ground
    -> Interpreter ground (Scope ground)
joinAllScopes [] s = return s
joinAllScopes (a:aa) s = do
    s' <- joinScopes a s
    joinAllScopes aa s'

data Module (ground :: GroundTypeKind) = MkModule
    { moduleDoc :: Tree (EntryDoc ground)
    , moduleScope :: Scope ground
    }

type InterpretContext :: GroundTypeKind -> Type
data InterpretContext ground = MkInterpretContext
    { icSourcePos :: SourcePos
    , icVarIDState :: VarIDState
    , icScope :: Scope ground
    , icNamespace :: Namespace
    , icSpecialVals :: SpecialVals ground
    , icModulePath :: [ModuleName]
    , icLoadModule :: ModuleName -> Interpreter ground (Maybe (Module ground))
    }

type InterpretState :: GroundTypeKind -> Type
data InterpretState ground = MkInterpretState
    { isTypeID :: TypeID
    , isModules :: Map ModuleName (Module ground)
    }

emptyInterpretState :: forall (ground :: GroundTypeKind). InterpretState ground
emptyInterpretState = let
    isTypeID = zeroTypeID
    isModules = mempty
    in MkInterpretState {..}

type Interpreter :: GroundTypeKind -> Type -> Type
newtype Interpreter ground a = MkInterpreter
    { unInterpreter :: ReaderT (InterpretContext ground) (StateT (InterpretState ground) InterpretResult) a
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

instance forall (ground :: GroundTypeKind). MonadThrow ErrorType (Interpreter ground) where
    throw err = do
        spos <- paramAsk sourcePosParam
        throw $ MkErrorMessage spos err mempty

instance forall (ground :: GroundTypeKind). MonadThrow ExpressionError (Interpreter ground) where
    throw err = throw $ ExpressionErrorError err

instance forall (ground :: GroundTypeKind) a. Semigroup a => Semigroup (Interpreter ground a) where
    (<>) = liftA2 (<>)

instance forall (ground :: GroundTypeKind) a. Monoid a => Monoid (Interpreter ground a) where
    mappend = (<>)
    mempty = pure mempty

contextParam :: forall (ground :: GroundTypeKind). Param (Interpreter ground) (InterpretContext ground)
contextParam = MkParam (MkInterpreter ask) $ \a (MkInterpreter m) -> MkInterpreter $ with a m

sourcePosParam :: forall (ground :: GroundTypeKind). Param (Interpreter ground) SourcePos
sourcePosParam = lensMapParam (\bfb a -> fmap (\b -> a {icSourcePos = b}) $ bfb $ icSourcePos a) contextParam

varIDStateParam :: forall (ground :: GroundTypeKind). Param (Interpreter ground) VarIDState
varIDStateParam = lensMapParam (\bfb a -> fmap (\b -> a {icVarIDState = b}) $ bfb $ icVarIDState a) contextParam

scopeParam :: forall (ground :: GroundTypeKind). Param (Interpreter ground) (Scope ground)
scopeParam = lensMapParam (\bfb a -> fmap (\b -> a {icScope = b}) $ bfb $ icScope a) contextParam

bindingsParam :: forall (ground :: GroundTypeKind). Param (Interpreter ground) (NameMap ground)
bindingsParam = lensMapParam (\bfb a -> fmap (\b -> a {scopeBindings = b}) $ bfb $ scopeBindings a) scopeParam

namespaceParam :: forall (ground :: GroundTypeKind). Param (Interpreter ground) Namespace
namespaceParam = lensMapParam (\bfb a -> fmap (\b -> a {icNamespace = b}) $ bfb $ icNamespace a) contextParam

specialValsParam :: forall (ground :: GroundTypeKind). Param (Interpreter ground) (SpecialVals ground)
specialValsParam = lensMapParam (\bfb a -> fmap (\b -> a {icSpecialVals = b}) $ bfb $ icSpecialVals a) contextParam

modulePathParam :: forall (ground :: GroundTypeKind). Param (Interpreter ground) [ModuleName]
modulePathParam = lensMapParam (\bfb a -> fmap (\b -> a {icModulePath = b}) $ bfb $ icModulePath a) contextParam

loadModuleParam ::
       forall (ground :: GroundTypeKind).
       Param (Interpreter ground) (ModuleName -> Interpreter ground (Maybe (Module ground)))
loadModuleParam = lensMapParam (\bfb a -> fmap (\b -> a {icLoadModule = b}) $ bfb $ icLoadModule a) contextParam

interpretStateRef :: forall (ground :: GroundTypeKind). Ref (Interpreter ground) (InterpretState ground)
interpretStateRef = let
    ref = liftRef stateRef
    in MkRef (MkInterpreter $ refGet ref) $ \a -> MkInterpreter $ refPut ref a

typeIDRef :: forall (ground :: GroundTypeKind). Ref (Interpreter ground) TypeID
typeIDRef = lensMapRef (\bfb a -> fmap (\b -> a {isTypeID = b}) $ bfb $ isTypeID a) interpretStateRef

modulesRef :: forall (ground :: GroundTypeKind). Ref (Interpreter ground) (Map ModuleName (Module ground))
modulesRef = lensMapRef (\bfb a -> fmap (\b -> a {isModules = b}) $ bfb $ isModules a) interpretStateRef

runInterpreter ::
       forall (ground :: GroundTypeKind) a.
       SourcePos
    -> (ModuleName -> Interpreter ground (Maybe (Module ground)))
    -> SpecialVals ground
    -> Interpreter ground a
    -> InterpretResult a
runInterpreter icSourcePos icLoadModule icSpecialVals qa = let
    icVarIDState = firstVarIDState
    icScope = emptyScope
    icModulePath = []
    icNamespace = RootNamespace
    in evalStateT (runReaderT (unInterpreter qa) $ MkInterpretContext {..}) emptyInterpretState

firstOf :: [a] -> (a -> Maybe b) -> Maybe b
firstOf [] _ = Nothing
firstOf (a:aa) amb =
    case amb a of
        Just b -> Just b
        Nothing -> firstOf aa amb

getCurrentNamespace :: forall (ground :: GroundTypeKind). Interpreter ground Namespace
getCurrentNamespace = paramAsk namespaceParam

namespacePriority :: forall (ground :: GroundTypeKind). Interpreter ground (NamespaceRef -> [Namespace])
namespacePriority = do
    curns <- getCurrentNamespace
    return $ namespaceConcatRefM $ toList $ namespaceAncestry curns

-- | For error messages and the like, doesn't need to be perfect.
getRenderFullName :: forall (ground :: GroundTypeKind). Interpreter ground (NamedText -> Text)
getRenderFullName = do
    curns <- getCurrentNamespace
    return $ runRelativeNamedText $ toList $ namespaceAncestry curns

throwWithName :: forall (ground :: GroundTypeKind) a. ((NamedText -> Text) -> ErrorType) -> Interpreter ground a
throwWithName err = do
    ntt <- getRenderFullName
    throw $ err ntt

getBindingMap :: forall (ground :: GroundTypeKind). Interpreter ground (FullNameRef -> Maybe (BindingInfo ground))
getBindingMap = do
    nspace <- paramAsk bindingsParam
    nsp <- namespacePriority
    return $ \(MkFullNameRef name nsn) ->
        firstOf (nsp nsn) $ \ns -> nameMapLookupBindingInfo nspace $ MkFullName name ns

lookupBinding ::
       forall (ground :: GroundTypeKind). Interpreter ground (FullNameRef -> Maybe (InterpreterBinding ground))
lookupBinding = do
    bindmap <- getBindingMap
    return $ \rname -> fmap biValue $ bindmap rname

lookupDebugBindingInfo ::
       forall (ground :: GroundTypeKind). IsInterpreterGroundType ground
    => FullNameRef
    -> Interpreter ground (FullName, String)
lookupDebugBindingInfo name = do
    bindmap <- getBindingMap
    case bindmap name of
        Nothing -> throw $ LookupRefNameUnknownError name
        Just b -> return $ (biName b, show $ biValue b)

checkPureExpression ::
       forall (ground :: GroundTypeKind).
       (IsInterpreterGroundType ground, Show VarID, AllConstraint Show (DolanType ground 'Negative))
    => TSSealedExpression (DolanTypeSystem ground)
    -> Interpreter ground ()
checkPureExpression expr = do
    _ <- tsEval @(DolanTypeSystem ground) expr
    return ()

checkPureBinding ::
       forall (ground :: GroundTypeKind).
       (IsInterpreterGroundType ground, Show VarID, AllConstraint Show (DolanType ground 'Negative))
    => InterpreterBinding ground
    -> Interpreter ground ()
checkPureBinding (ValueBinding expr _) = checkPureExpression expr
checkPureBinding _ = return ()

exportNames ::
       forall (ground :: GroundTypeKind). (Show VarID, AllConstraint Show (DolanType ground 'Negative))
    => [FullNameRef]
    -> Interpreter ground [BindingInfo ground]
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

exportNamespace :: forall (ground :: GroundTypeKind). [Namespace] -> Interpreter ground [BindingInfo ground]
exportNamespace cnss = do
    MkNameMap nspace <- paramAsk bindingsParam
    let
        toBI :: (FullName, (RawMarkdown, InterpreterBinding ground)) -> Maybe (BindingInfo ground)
        toBI (biName@(MkFullName _ ns), (biDocumentation, biValue)) = do
            _ <- choice $ fmap (\cns -> namespaceWithin cns ns) cnss
            return MkBindingInfo {..}
    return $ mapMaybe toBI $ mapToList nspace

exportScope ::
       forall (ground :: GroundTypeKind).
       (IsInterpreterGroundType ground, Show VarID, AllConstraint Show (DolanType ground 'Negative))
    => [Namespace]
    -> [FullNameRef]
    -> Interpreter ground ([FullName], Scope ground)
exportScope nsns names = do
    MkScope _ subtypes <- paramAsk scopeParam
    nsbindss <- exportNamespace nsns
    nbinds <- exportNames names
    let binds = nsbindss <> nbinds
    for_ binds $ \bi -> checkPureBinding $ biValue bi
    return $ (fmap biName binds, MkScope (bindingInfosToNameMap binds) subtypes)

type ScopeInterpreter (ground :: GroundTypeKind) = TransformT (Interpreter ground)

scopeRef :: forall (ground :: GroundTypeKind). Ref (ScopeInterpreter ground) (Scope ground)
scopeRef = transformParamRef scopeParam

bindingsRef :: forall (ground :: GroundTypeKind). Ref (ScopeInterpreter ground) (NameMap ground)
bindingsRef = transformParamRef bindingsParam

namespaceRef :: forall (ground :: GroundTypeKind). Ref (ScopeInterpreter ground) Namespace
namespaceRef = transformParamRef namespaceParam

varIDStateRef :: forall (ground :: GroundTypeKind). Ref (ScopeInterpreter ground) VarIDState
varIDStateRef = transformParamRef varIDStateParam

scopeSourcePos :: forall (ground :: GroundTypeKind). SourcePos -> ScopeInterpreter ground ()
scopeSourcePos = refPut (transformParamRef sourcePosParam)

allocateVar ::
       forall (ground :: GroundTypeKind). IsInterpreterGroundType ground
    => Maybe FullName
    -> ScopeInterpreter ground (FullName, VarID)
allocateVar mname = do
    vs <- refGet varIDStateRef
    let
        (vid, biName) =
            case mname of
                Just name -> (mkVarID vs name, name)
                Nothing -> mkUniqueVarID vs
        biDocumentation = fromString "variable"
        biValue = ValueBinding (tsVar @(DolanTypeSystem ground) vid) Nothing
        insertScope = MkScope (bindingInfoToNameMap MkBindingInfo {..}) mempty
    refPut varIDStateRef $ nextVarIDState vs
    refModifyM scopeRef $ \oldScope -> lift $ joinScopes insertScope oldScope
    return (biName, vid)

getRestore :: Monad m => Ref m a -> m (m ())
getRestore r = do
    old <- refGet r
    return $ refPut r old

withNamespace :: forall (ground :: GroundTypeKind). Namespace -> ScopeInterpreter ground (ScopeInterpreter ground ())
withNamespace ns = do
    nrestore <- getRestore namespaceRef
    refPut namespaceRef ns
    return nrestore

usingNamespace ::
       forall (ground :: GroundTypeKind). Namespace -> Namespace -> (FullNameRef -> Bool) -> ScopeInterpreter ground ()
usingNamespace sourcens destns ff = refModify bindingsRef $ nameMapLookupNamespace sourcens destns ff

registerScope ::
       forall (ground :: GroundTypeKind). IsInterpreterGroundType ground
    => Scope ground
    -> ScopeInterpreter ground ()
registerScope insertScope = refModifyM scopeRef $ \oldScope -> lift $ joinScopes insertScope oldScope

getCycle :: ModuleName -> [ModuleName] -> Maybe (NonEmpty ModuleName)
getCycle _ [] = Nothing
getCycle mn (n:nn)
    | mn == n = Just $ n :| nn
getCycle mn (_:nn) = getCycle mn nn

loadModuleInScope :: forall (ground :: GroundTypeKind). ModuleName -> Interpreter ground (Maybe (Module ground))
loadModuleInScope mname =
    paramWith sourcePosParam (initialPos "<unknown>") $
    paramWith scopeParam emptyScope $
    paramLocal modulePathParam (\path -> path <> [mname]) $ do
        loadModule <- paramAsk loadModuleParam
        loadModule mname

getModule :: forall (ground :: GroundTypeKind). ModuleName -> Interpreter ground (Module ground)
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
       forall (ground :: GroundTypeKind). FullName -> DocInterpreterBinding ground -> ScopeInterpreter ground ()
registerBinding name db = registerBindings $ singletonMap name db

getSubtypeScope :: forall (ground :: GroundTypeKind). SubtypeConversionEntry ground -> Interpreter ground (Scope ground)
getSubtypeScope sce = do
    key <- liftIO newUnique
    return $ emptyScope {scopeSubtypes = singletonMap key sce}

registerBindings ::
       forall (ground :: GroundTypeKind). [(FullName, DocInterpreterBinding ground)] -> ScopeInterpreter ground ()
registerBindings bb = do
    let newBindings = MkNameMap $ mapFromList bb
    refModify bindingsRef $ \oldBindings -> oldBindings <> newBindings

getSpecialVals :: forall (ground :: GroundTypeKind). Interpreter ground (SpecialVals ground)
getSpecialVals = paramAsk specialValsParam

data BoundValue (ground :: GroundTypeKind)
    = ValueBoundValue (TSSealedExpression (DolanTypeSystem ground))
    | RecordBoundValue (RecordConstructor ground)

lookupLetBinding ::
       forall (ground :: GroundTypeKind). IsInterpreterGroundType ground
    => Interpreter ground (FullNameRef -> Maybe (BoundValue ground))
lookupLetBinding = do
    mb <- lookupBinding
    return $ \name ->
        case mb name of
            Just (ValueBinding exp _) -> Just $ ValueBoundValue exp
            Just (RecordConstructorBinding rc _) -> Just $ RecordBoundValue rc
            _ -> Nothing

registerLetBindings ::
       forall (ground :: GroundTypeKind). IsInterpreterGroundType ground
    => [(FullName, RawMarkdown, TSSealedExpression (DolanTypeSystem ground))]
    -> ScopeInterpreter ground ()
registerLetBindings bb = registerBindings $ fmap (\(nref, doc, exp) -> (nref, (doc, ValueBinding exp Nothing))) bb

registerLetBinding ::
       forall (ground :: GroundTypeKind). IsInterpreterGroundType ground
    => FullName
    -> RawMarkdown
    -> TSSealedExpression (DolanTypeSystem ground)
    -> ScopeInterpreter ground ()
registerLetBinding name doc expr = registerLetBindings $ pure (name, doc, expr)

registerMatchBindings ::
       forall (ground :: GroundTypeKind). IsInterpreterGroundType ground
    => TSMatch (DolanTypeSystem ground)
    -> ScopeInterpreter ground ()
registerMatchBindings match = do
    bb <-
        for (tsMatchBindings @(DolanTypeSystem ground) match) $ \(MkSealedExpression pw expr) ->
            case pw of
                ValuePatternWitness wvar tw -> do
                    fn <-
                        case varIdNameRef wvar of
                            SuccessResult fn -> return fn
                            FailureResult fn -> lift $ throw $ KnownIssueError 0 $ "bad match var: " <> toText fn
                    return (fn, ("match", ValueBinding (MkSealedExpression tw expr) Nothing))
                TypePatternWitness fn -> do
                    sgt <- lift $ createGroundType fn
                    return (fn, ("match", TypeBinding sgt))
    registerBindings bb

lookupSpecialForm ::
       forall (ground :: GroundTypeKind).
       FullNameRef
    -> Interpreter ground (SpecialForm (DolanTypeSystem ground) (Interpreter ground))
lookupSpecialForm name = do
    mb <- lookupBinding
    case mb name of
        Just (SpecialFormBinding sf) -> return sf
        _ -> throw $ LookupSpecialFormUnknownError name

lookupBoundTypeM :: forall (ground :: GroundTypeKind). Interpreter ground (FullNameRef -> Maybe (SomeGroundType ground))
lookupBoundTypeM = do
    mb <- lookupBinding
    return $ \name ->
        case mb name of
            Just (TypeBinding t) -> Just t
            _ -> Nothing

lookupBoundType :: forall (ground :: GroundTypeKind). FullNameRef -> Interpreter ground (SomeGroundType ground)
lookupBoundType name = do
    mnt <- lookupBoundTypeM
    case mnt name of
        Just nt -> return nt
        Nothing -> throw $ LookupTypeUnknownError name

lookupPatternConstructorM ::
       forall (ground :: GroundTypeKind).
       Interpreter ground (FullNameRef -> Maybe (Either (TSExpressionPatternConstructor (DolanTypeSystem ground)) (RecordPattern ground)))
lookupPatternConstructorM = do
    mb <- lookupBinding
    return $ \name ->
        case mb name of
            Just (ValueBinding _ (Just pc)) -> Just $ Left pc
            Just (RecordConstructorBinding _ rp) -> Just $ Right rp
            _ -> Nothing

lookupPatternConstructor ::
       forall (ground :: GroundTypeKind).
       FullNameRef
    -> Interpreter ground (Either (TSExpressionPatternConstructor (DolanTypeSystem ground)) (RecordPattern ground))
lookupPatternConstructor name = do
    ma <- lookupPatternConstructorM
    case ma name of
        Just a -> return a
        Nothing -> throw $ LookupConstructorUnknownError name

newTypeID :: forall (ground :: GroundTypeKind). Interpreter ground (Some TypeIDType)
newTypeID = do
    tid <- refGet typeIDRef
    refPut typeIDRef $ succTypeID tid
    return $ valueToSome tid

withNewTypeID ::
       forall (ground :: GroundTypeKind) a. (forall tid. TypeIDType tid -> Interpreter ground a) -> Interpreter ground a
withNewTypeID call = do
    stid <- newTypeID
    case stid of
        MkSome tid -> call tid

checkName :: forall (ground :: GroundTypeKind). FullName -> ScopeInterpreter ground ()
checkName name = do
    mnt <- lift lookupBinding
    case mnt $ fullNameRef name of
        Just _ -> lift $ throw $ DeclareTypeDuplicateError name
        Nothing -> return ()

registerBoundType ::
       forall (ground :: GroundTypeKind). FullName -> RawMarkdown -> SomeGroundType ground -> ScopeInterpreter ground ()
registerBoundType name doc t = do
    checkName name
    registerBinding name (doc, TypeBinding t)

registerType ::
       forall (ground :: GroundTypeKind) dv t. FullName -> RawMarkdown -> ground dv t -> ScopeInterpreter ground ()
registerType name doc t = do
    checkName name
    registerBoundType name doc $ MkSomeGroundType t

type ScopeFixBox (ground :: GroundTypeKind) = FixBox (ScopeInterpreter ground)

registerPatternConstructor ::
       forall (ground :: GroundTypeKind).
       FullName
    -> RawMarkdown
    -> TSSealedExpression (DolanTypeSystem ground)
    -> TSExpressionPatternConstructor (DolanTypeSystem ground)
    -> ScopeInterpreter ground ()
registerPatternConstructor name doc exp pc = do
    checkName name
    registerBinding name $ (doc, ValueBinding exp $ Just pc)

registerRecord ::
       forall (ground :: GroundTypeKind).
       FullName
    -> RawMarkdown
    -> RecordConstructor ground
    -> RecordPattern ground
    -> ScopeInterpreter ground ()
registerRecord name doc rc rp = do
    checkName name
    registerBinding name $ (doc, RecordConstructorBinding rc rp)

registerSubtypeConversion ::
       forall (ground :: GroundTypeKind). IsInterpreterGroundType ground
    => SubtypeConversionEntry ground
    -> ScopeInterpreter ground ()
registerSubtypeConversion sce = do
    newscope <- lift $ getSubtypeScope sce
    registerScope newscope

getSubtypeConversions :: forall (ground :: GroundTypeKind). Interpreter ground [SubtypeConversionEntry ground]
getSubtypeConversions = fmap (fmap snd . mapToList . scopeSubtypes) $ paramAsk scopeParam
