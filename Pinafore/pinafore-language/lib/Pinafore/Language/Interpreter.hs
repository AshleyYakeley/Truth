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
    , exportNames
    , exportScope
    , getModule
    , SourcePos
    , initialPos
    , Interpreter
    , ScopeInterpreter
    , sourcePosParam
    , SpecialVals(..)
    , getSpecialVals
    , lookupLetBinding
    , registerLetBindings
    , registerLetBinding
    , unregisterBindings
    , bindingsScope
    , getSubtypeScope
    , lookupSpecialForm
    , lookupBoundType
    , newTypeID
    , withNewTypeID
    , registerBoundType
    , registerType
    , ScopeFixBox
    , DocInterpreterBinding
    , lookupDocBinding
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

type DocInterpreterBinding ts = (Markdown, InterpreterBinding ts)

data Scope (ts :: Type) = MkScope
    { scopeBindings :: Map Name (DocInterpreterBinding ts)
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
        bb = scopeBindings a <> scopeBindings b
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
    in evalStateT (runReaderT (unInterpreter qa) $ MkInterpretContext {..}) emptyInterpretState

allocateVar ::
       forall ts. HasInterpreter ts
    => Name
    -> ScopeInterpreter ts VarID
allocateVar n =
    MkTransformT $ \f -> do
        vs <- paramAsk varIDStateParam
        let
            vid = mkVarID vs n
            insertScope = MkScope (singletonMap n (plainMarkdown "variable", LambdaBinding vid)) mempty
        paramWith varIDStateParam (nextVarIDState vs) $ do
            oldScope <- paramAsk scopeParam
            newScope <- joinScopes insertScope oldScope
            paramWith scopeParam newScope $ f vid

purifyExpression ::
       forall ts. (Show (TSVarID ts), AllConstraint Show (TSNegWitness ts))
    => TSSealedExpression ts
    -> Interpreter ts (TSSealedExpression ts)
purifyExpression expr = do
    _ <- tsEval @ts expr
    return expr

purifyBinding ::
       (Show (TSVarID ts), AllConstraint Show (TSNegWitness ts))
    => InterpreterBinding ts
    -> Interpreter ts (InterpreterBinding ts)
purifyBinding (ValueBinding expr mpatc) = do
    expr' <- purifyExpression expr
    return $ ValueBinding expr' mpatc
purifyBinding b = return b

exportName ::
       forall ts. (Show (TSVarID ts), AllConstraint Show (TSNegWitness ts))
    => Name
    -> Interpreter ts (Maybe (DocInterpreterBinding ts))
exportName name = do
    MkScope bindings _ <- paramAsk scopeParam
    for (lookup name bindings) $ \(doc, b) -> do
        b' <- purifyBinding b
        return (doc, b')

exportNames ::
       forall ts. (Show (TSVarID ts), AllConstraint Show (TSNegWitness ts))
    => [Name]
    -> Interpreter ts [(Name, DocInterpreterBinding ts)]
exportNames names = do
    nbs <-
        for names $ \name -> do
            mdib <- exportName name
            return $ (name, mdib)
    case [name | (name, Nothing) <- nbs] of
        [] -> return [(name, dib) | (name, Just dib) <- nbs]
        (n:nn) -> throw $ LookupNamesUnknownError $ n :| nn

exportScope ::
       forall ts. (Show (TSVarID ts), AllConstraint Show (TSNegWitness ts))
    => [Name]
    -> Interpreter ts (Scope ts)
exportScope names = do
    MkScope _ subtypes <- paramAsk scopeParam
    goodbinds <- exportNames names
    return $ MkScope (mapFromList goodbinds) subtypes

type ScopeInterpreter ts = TransformT (Interpreter ts)

scopeRef :: Ref (ScopeInterpreter ts) (Scope ts)
scopeRef = transformParamRef scopeParam

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

registerBinding :: Name -> DocInterpreterBinding ts -> ScopeInterpreter ts ()
registerBinding name db = refModify scopeRef $ \tc -> tc {scopeBindings = insertMapLazy name db $ scopeBindings tc}

bindingsScope :: Map Name (DocInterpreterBinding ts) -> Scope ts
bindingsScope bb = emptyScope {scopeBindings = bb}

getSubtypeScope :: SubtypeConversionEntry (InterpreterGroundType ts) -> Interpreter ts (Scope ts)
getSubtypeScope sce = do
    key <- liftIO newUnique
    return $ emptyScope {scopeSubtypes = singletonMap key sce}

registerBindings ::
       forall ts. HasInterpreter ts
    => Map Name (DocInterpreterBinding ts)
    -> ScopeInterpreter ts ()
registerBindings bb = registerScope $ bindingsScope bb

unregisterBindings :: [Name] -> ScopeInterpreter ts ()
unregisterBindings nn = refModify scopeRef $ \tc -> tc {scopeBindings = deletesMap nn $ scopeBindings tc}

lookupDocBinding :: ReferenceName -> Interpreter ts (Maybe (DocInterpreterBinding ts))
lookupDocBinding (UnqualifiedReferenceName name) = do
    (scopeBindings -> names) <- paramAsk scopeParam
    return $ lookup name names
lookupDocBinding (QualifiedReferenceName mname name) = do
    modl <- getModule mname
    return $ lookup name $ scopeBindings $ moduleScope modl

lookupBinding :: ReferenceName -> Interpreter ts (Maybe (InterpreterBinding ts))
lookupBinding rname = fmap (fmap snd) $ lookupDocBinding rname

getSpecialVals :: Interpreter ts (SpecialVals ts)
getSpecialVals = paramAsk specialValsParam

lookupLetBinding :: ReferenceName -> Interpreter ts (Maybe (Either VarID (TSSealedExpression ts)))
lookupLetBinding name = do
    mb <- lookupBinding name
    case mb of
        Just (ValueBinding exp _) -> return $ Just $ Right exp
        Just (LambdaBinding v) -> return $ Just $ Left v
        _ -> return Nothing

registerLetBindings ::
       forall ts. HasInterpreter ts
    => Map Name (Markdown, TSSealedExpression ts)
    -> ScopeInterpreter ts ()
registerLetBindings bb = registerBindings $ fmap (\(doc, exp) -> (doc, ValueBinding exp Nothing)) bb

registerLetBinding ::
       forall ts. HasInterpreter ts
    => Name
    -> Markdown
    -> TSSealedExpression ts
    -> ScopeInterpreter ts ()
registerLetBinding name doc expr = registerLetBindings $ singletonMap name (doc, expr)

lookupSpecialForm :: ReferenceName -> Interpreter ts (SpecialForm ts (Interpreter ts))
lookupSpecialForm name = do
    mb <- lookupBinding name
    case mb of
        Just (SpecialFormBinding sf) -> return sf
        _ -> throw $ LookupSpecialFormUnknownError name

lookupBoundTypeM :: ReferenceName -> Interpreter ts (Maybe (InterpreterBoundType ts))
lookupBoundTypeM name = do
    mb <- lookupBinding name
    return $
        case mb of
            Just (TypeBinding t) -> Just t
            _ -> Nothing

lookupBoundType :: ReferenceName -> Interpreter ts (InterpreterBoundType ts)
lookupBoundType name = do
    mnt <- lookupBoundTypeM name
    case mnt of
        Just nt -> return nt
        Nothing -> throw $ LookupTypeUnknownError name

lookupPatternConstructorM :: ReferenceName -> Interpreter ts (Maybe (TSExpressionPatternConstructor ts))
lookupPatternConstructorM name = do
    mb <- lookupBinding name
    return $
        case mb of
            Just (ValueBinding _ (Just pc)) -> Just pc
            _ -> Nothing

lookupPatternConstructor :: ReferenceName -> Interpreter ts (TSExpressionPatternConstructor ts)
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

registerBoundType :: Name -> Markdown -> InterpreterBoundType ts -> ScopeInterpreter ts ()
registerBoundType name doc t = do
    mnt <- lift $ lookupBinding $ UnqualifiedReferenceName name
    case mnt of
        Just _ -> lift $ throw $ DeclareTypeDuplicateError name
        Nothing -> registerBinding name (doc, TypeBinding t)

registerType :: Name -> Markdown -> InterpreterGroundType ts dv t -> ScopeInterpreter ts ()
registerType name doc t = registerBoundType name doc $ MkSomeGroundType t

type ScopeFixBox ts = FixBox (ScopeInterpreter ts)

registerPatternConstructor ::
       Name -> Markdown -> TSSealedExpression ts -> TSExpressionPatternConstructor ts -> ScopeInterpreter ts ()
registerPatternConstructor name doc exp pc = do
    ma <- lift $ lookupBinding $ UnqualifiedReferenceName name
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
