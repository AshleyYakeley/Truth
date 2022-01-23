module Pinafore.Language.Interpreter
    ( InterpreterGroundType
    , InterpreterFamilyType
    , BoundType(..)
    , runInterpreter
    , allocateVar
    , EntryDoc
    , Module(..)
    , Scope
    , importScope
    , exportNames
    , exportScope
    , getModule
    , SourcePos
    , initialPos
    , Interpreter
    , sourcePosParam
    , SpecialVals(..)
    , getSpecialVals
    , lookupLetBinding
    , withNewLetBindings
    , withRemovedBindings
    , bindingsScope
    , getSubtypesScope
    , lookupSpecialForm
    , lookupBoundType
    , newTypeID
    , TypeFixBox
    , mkTypeFixBox
    , registerTypeName
    , registerRecursiveTypeNames
    , DocInterpreterBinding
    , lookupDocBinding
    , InterpreterBinding(..)
    , lookupPatternConstructor
    , withNewPatternConstructor
    , withSubtypeConversions
    , getSubtypeConversions
    ) where

import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.Error
import Pinafore.Language.Name
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type.Identified
import Pinafore.Language.VarID
import Pinafore.Markdown
import Shapes
import Text.Parsec.Pos (SourcePos, initialPos)

type family InterpreterGroundType (ts :: Type) :: GroundTypeKind

type family InterpreterFamilyType (ts :: Type) :: forall k. k -> Type

data BoundType (ts :: Type) where
    MkBoundType
        :: forall (ts :: Type) (dv :: DolanVariance) (t :: DolanVarianceKind dv).
           InterpreterGroundType ts dv t
        -> BoundType ts

newtype SpecialVals (ts :: Type) = MkSpecialVals
    { specialEvaluate :: forall t. TSPosWitness ts t -> Text -> PinaforeAction (Either Text t)
        -- ^ in Action because this can do things like import files
    }

data InterpreterBinding (ts :: Type)
    = LambdaBinding VarID
    | ValueBinding (TSSealedExpression ts)
                   (Maybe (TSPatternConstructor ts))
    | TypeBinding (BoundType ts)
    | SpecialFormBinding (SpecialForm ts (Interpreter ts))

type DocInterpreterBinding ts = (Markdown, InterpreterBinding ts)

data Scope (ts :: Type) = MkScope
    { scopeBindings :: Map Name (DocInterpreterBinding ts)
    , scopeSubtypes :: HashMap Unique (SubtypeConversionEntry (InterpreterGroundType ts))
    }

instance Semigroup (Scope ts) where
    MkScope b1 s1 <> MkScope b2 s2 = MkScope (b1 <> b2) (s1 <> s2)

instance Monoid (Scope ts) where
    mempty = MkScope mempty mempty

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
    } deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, MonadFix, MonadTunnelIO)

instance MonadThrow PinaforeError (Interpreter ts) where
    throw err = MkInterpreter $ throw err

instance MonadCatch PinaforeError (Interpreter ts) where
    catch (MkInterpreter ma) ema = MkInterpreter $ catch ma $ \e -> unInterpreter $ ema e

instance MonadThrow ErrorMessage (Interpreter ts) where
    throw = throwErrorMessage

instance MonadThrow ExpressionError (Interpreter ts) where
    throw err = throw $ ExpressionErrorError err

instance MonadThrow ErrorType (Interpreter ts) where
    throw err = do
        spos <- askD sourcePosParam
        throw $ MkErrorMessage spos err mempty

instance Semigroup a => Semigroup (Interpreter ts a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Interpreter ts a) where
    mappend = (<>)
    mempty = pure mempty

contextParam :: Param (Interpreter ts) (InterpretContext ts)
contextParam = MkParam {askD = MkInterpreter ask, localD = \aa (MkInterpreter m) -> MkInterpreter $ local aa m}

sourcePosParam :: Param (Interpreter ts) SourcePos
sourcePosParam = mapParam (\bfb a -> fmap (\b -> a {icSourcePos = b}) $ bfb $ icSourcePos a) contextParam

varIDStateParam :: Param (Interpreter ts) VarIDState
varIDStateParam = mapParam (\bfb a -> fmap (\b -> a {icVarIDState = b}) $ bfb $ icVarIDState a) contextParam

scopeParam :: Param (Interpreter ts) (Scope ts)
scopeParam = mapParam (\bfb a -> fmap (\b -> a {icScope = b}) $ bfb $ icScope a) contextParam

specialValsParam :: Param (Interpreter ts) (SpecialVals ts)
specialValsParam = mapParam (\bfb a -> fmap (\b -> a {icSpecialVals = b}) $ bfb $ icSpecialVals a) contextParam

modulePathParam :: Param (Interpreter ts) [ModuleName]
modulePathParam = mapParam (\bfb a -> fmap (\b -> a {icModulePath = b}) $ bfb $ icModulePath a) contextParam

loadModuleParam :: Param (Interpreter ts) (ModuleName -> Interpreter ts (Maybe (Module ts)))
loadModuleParam = mapParam (\bfb a -> fmap (\b -> a {icLoadModule = b}) $ bfb $ icLoadModule a) contextParam

interpretStateRef :: Ref (Interpreter ts) (InterpretState ts)
interpretStateRef = let
    ref = liftRef stateRef
    in MkRef {getD = MkInterpreter $ getD ref, modifyD = \aa -> MkInterpreter $ modifyD ref aa}

typeIDRef :: Ref (Interpreter ts) TypeID
typeIDRef = mapRef (\bfb a -> fmap (\b -> a {isTypeID = b}) $ bfb $ isTypeID a) interpretStateRef

modulesRef :: Ref (Interpreter ts) (Map ModuleName (Module ts))
modulesRef = mapRef (\bfb a -> fmap (\b -> a {isModules = b}) $ bfb $ isModules a) interpretStateRef

runInterpreter ::
       SourcePos
    -> (ModuleName -> Interpreter ts (Maybe (Module ts)))
    -> SpecialVals ts
    -> Interpreter ts a
    -> InterpretResult a
runInterpreter icSourcePos icLoadModule icSpecialVals qa = let
    icVarIDState = firstVarIDState
    icScope = mempty
    icModulePath = []
    in evalStateT (runReaderT (unInterpreter qa) $ MkInterpretContext {..}) emptyInterpretState

allocateVar :: Name -> (VarID -> Interpreter ts a) -> Interpreter ts a
allocateVar n f = do
    vs <- askD varIDStateParam
    let
        vid = mkVarID vs n
        newscope = MkScope (singletonMap n (plainMarkdown "variable", LambdaBinding vid)) mempty
    withD varIDStateParam (nextVarIDState vs) $ localD scopeParam (\scope -> newscope <> scope) $ f vid

purifyExpression ::
       forall ts. (Show (TSVarID ts), AllWitnessConstraint Show (TSNegWitness ts))
    => TSSealedExpression ts
    -> Interpreter ts (TSSealedExpression ts)
purifyExpression expr = do
    _ <- tsEval @ts expr
    return expr

purifyBinding ::
       (Show (TSVarID ts), AllWitnessConstraint Show (TSNegWitness ts))
    => InterpreterBinding ts
    -> Interpreter ts (InterpreterBinding ts)
purifyBinding (ValueBinding expr mpatc) = do
    expr' <- purifyExpression expr
    return $ ValueBinding expr' mpatc
purifyBinding b = return b

exportName ::
       forall ts. (Show (TSVarID ts), AllWitnessConstraint Show (TSNegWitness ts))
    => Name
    -> Interpreter ts (Maybe (DocInterpreterBinding ts))
exportName name = do
    MkScope bindings _ <- askD scopeParam
    for (lookup name bindings) $ \(doc, b) -> do
        b' <- purifyBinding b
        return (doc, b')

exportNames ::
       forall ts. (Show (TSVarID ts), AllWitnessConstraint Show (TSNegWitness ts))
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
       forall ts. (Show (TSVarID ts), AllWitnessConstraint Show (TSNegWitness ts))
    => [Name]
    -> Interpreter ts (Scope ts)
exportScope names = do
    MkScope _ subtypes <- askD scopeParam
    goodbinds <- exportNames names
    return $ MkScope (mapFromList goodbinds) subtypes

importScope :: Scope ts -> Interpreter ts --> Interpreter ts
importScope newscope = localD scopeParam $ \oldscope -> newscope <> oldscope

getCycle :: ModuleName -> [ModuleName] -> Maybe (NonEmpty ModuleName)
getCycle _ [] = Nothing
getCycle mn (n:nn)
    | mn == n = Just $ n :| nn
getCycle mn (_:nn) = getCycle mn nn

loadModuleInScope :: forall ts. ModuleName -> Interpreter ts (Maybe (Module ts))
loadModuleInScope mname =
    withD sourcePosParam (initialPos "<unknown>") $
    withD scopeParam mempty $
    localD modulePathParam (\path -> path <> [mname]) $ do
        loadModule <- askD loadModuleParam
        loadModule mname

getModule :: ModuleName -> Interpreter ts (Module ts)
getModule mname = do
    mods <- getD modulesRef
    case lookup mname mods of
        Just m -> return m
        Nothing -> do
            mpath <- askD modulePathParam
            case getCycle mname mpath of
                Just mnames -> throw $ ModuleCycleError mnames
                Nothing -> do
                    mm <- loadModuleInScope mname
                    case mm of
                        Just m -> do
                            modifyD modulesRef $ insertMap mname m
                            return m
                        Nothing -> throw $ ModuleNotFoundError mname

type InterpreterEndo ts = CatEndo WMFunction (Interpreter ts)

withNewBinding :: Name -> DocInterpreterBinding ts -> Interpreter ts --> Interpreter ts
withNewBinding name db = localD scopeParam $ \tc -> tc {scopeBindings = insertMapLazy name db $ scopeBindings tc}

bindingsScope :: Map Name (DocInterpreterBinding ts) -> Scope ts
bindingsScope bb = mempty {scopeBindings = bb}

getSubtypesScope :: [SubtypeConversionEntry (InterpreterGroundType ts)] -> IO (Scope ts)
getSubtypesScope newscs = do
    pairs <-
        for newscs $ \newsc -> do
            key <- newUnique
            return (key, newsc)
    return $ mempty {scopeSubtypes = mapFromList pairs}

withNewBindings :: Map Name (DocInterpreterBinding ts) -> Interpreter ts --> Interpreter ts
withNewBindings bb = importScope $ bindingsScope bb

withRemovedBindings :: [Name] -> Interpreter ts --> Interpreter ts
withRemovedBindings nn = localD scopeParam $ \tc -> tc {scopeBindings = deletesMap nn $ scopeBindings tc}

lookupDocBinding :: ReferenceName -> Interpreter ts (Maybe (DocInterpreterBinding ts))
lookupDocBinding (UnqualifiedReferenceName name) = do
    (scopeBindings -> names) <- askD scopeParam
    return $ lookup name names
lookupDocBinding (QualifiedReferenceName mname name) = do
    modl <- getModule mname
    return $ lookup name $ scopeBindings $ moduleScope modl

lookupBinding :: ReferenceName -> Interpreter ts (Maybe (InterpreterBinding ts))
lookupBinding rname = fmap (fmap snd) $ lookupDocBinding rname

getSpecialVals :: Interpreter ts (SpecialVals ts)
getSpecialVals = askD specialValsParam

lookupLetBinding :: ReferenceName -> Interpreter ts (Maybe (Either VarID (TSSealedExpression ts)))
lookupLetBinding name = do
    mb <- lookupBinding name
    case mb of
        Just (ValueBinding exp _) -> return $ Just $ Right exp
        Just (LambdaBinding v) -> return $ Just $ Left v
        _ -> return Nothing

withNewLetBindings :: Map Name (Markdown, TSSealedExpression ts) -> Interpreter ts --> Interpreter ts
withNewLetBindings bb = withNewBindings $ fmap (\(doc, exp) -> (doc, ValueBinding exp Nothing)) bb

lookupSpecialForm :: ReferenceName -> Interpreter ts (SpecialForm ts (Interpreter ts))
lookupSpecialForm name = do
    mb <- lookupBinding name
    case mb of
        Just (SpecialFormBinding sf) -> return sf
        _ -> throw $ LookupSpecialFormUnknownError name

lookupBoundTypeM :: ReferenceName -> Interpreter ts (Maybe (BoundType ts))
lookupBoundTypeM name = do
    mb <- lookupBinding name
    return $
        case mb of
            Just (TypeBinding t) -> Just t
            _ -> Nothing

lookupBoundType :: ReferenceName -> Interpreter ts (BoundType ts)
lookupBoundType name = do
    mnt <- lookupBoundTypeM name
    case mnt of
        Just nt -> return nt
        Nothing -> throw $ LookupTypeUnknownError name

lookupPatternConstructorM :: ReferenceName -> Interpreter ts (Maybe (TSPatternConstructor ts))
lookupPatternConstructorM name = do
    mb <- lookupBinding name
    return $
        case mb of
            Just (ValueBinding _ (Just pc)) -> Just pc
            _ -> Nothing

lookupPatternConstructor :: ReferenceName -> Interpreter ts (TSPatternConstructor ts)
lookupPatternConstructor name = do
    ma <- lookupPatternConstructorM name
    case ma of
        Just a -> return a
        Nothing -> throw $ LookupConstructorUnknownError name

newTypeID :: (forall tid. TypeIDType tid -> a) -> Interpreter ts a
newTypeID call = do
    tid <- getD typeIDRef
    putD typeIDRef $ succTypeID tid
    return $ valueToWitness tid call

registerType :: Name -> Markdown -> BoundType ts -> WMFunction (Interpreter ts) (Interpreter ts)
registerType name doc t =
    MkWMFunction $ \mta -> do
        mnt <- lookupBoundTypeM $ UnqualifiedReferenceName name
        case mnt of
            Just _ -> throw $ DeclareTypeDuplicateError name
            Nothing -> withNewBinding name (doc, TypeBinding t) mta

type TypeFixBox ts = FixBox (Interpreter ts)

mkTypeFixBox :: Name -> Markdown -> (t -> BoundType ts) -> Interpreter ts (t, x) -> TypeFixBox ts x
mkTypeFixBox name doc ttype mtx = mkFixBox (\t -> registerType name doc $ ttype t) mtx

runWriterInterpreterMF ::
       (forall a. Interpreter ts a -> Interpreter ts (a, x))
    -> Interpreter ts (WMFunction (Interpreter ts) (Interpreter ts), x)
runWriterInterpreterMF mf = do
    (sc, xx) <- mf $ askD scopeParam
    return (MkWMFunction $ withD scopeParam sc, xx)

registerRecursiveTypeNames ::
       Monoid x => [TypeFixBox ts x] -> Interpreter ts (WMFunction (Interpreter ts) (Interpreter ts), x)
registerRecursiveTypeNames tboxes = runWriterInterpreterMF $ boxesFix tboxes

registerTypeName :: TypeFixBox ts x -> Interpreter ts (WMFunction (Interpreter ts) (Interpreter ts), x)
registerTypeName tbox = runWriterInterpreterMF $ boxSeq tbox

withNewPatternConstructor ::
       Name -> Markdown -> TSSealedExpression ts -> TSPatternConstructor ts -> Interpreter ts (InterpreterEndo ts)
withNewPatternConstructor name doc exp pc = do
    ma <- lookupPatternConstructorM $ UnqualifiedReferenceName name
    case ma of
        Just _ -> throw $ DeclareConstructorDuplicateError name
        Nothing -> return $ MkCatEndo $ MkWMFunction $ withNewBinding name $ (doc, ValueBinding exp $ Just pc)

withSubtypeConversions :: [SubtypeConversionEntry (InterpreterGroundType ts)] -> Interpreter ts a -> Interpreter ts a
withSubtypeConversions newscs ma = do
    newscope <- liftIO $ getSubtypesScope newscs
    importScope newscope ma

getSubtypeConversions :: Interpreter ts [SubtypeConversionEntry (InterpreterGroundType ts)]
getSubtypeConversions = fmap (fmap snd . mapToList . scopeSubtypes) $ askD scopeParam
