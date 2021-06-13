module Pinafore.Language.Interpreter
    ( InterpreterGroundType
    , InterpreterProvidedType
    , InterpreterClosedEntityType
    , BoundType(..)
    , Interpreter
    , runInterpreter
    , liftInterpreter
    , Scope
    , importScope
    , exportScope
    , importModule
    , SourcePos
    , SourceInterpreter
    , askSourcePos
    , localSourcePos
    , remonadSourcePos
    , runSourcePos
    , liftSourcePos
    , mapSourcePos
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
    , TypeBox(..)
    , registerTypeNames
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
import Pinafore.Language.Error
import Pinafore.Language.Name
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type.Identified
import Pinafore.Markdown
import Shapes
import Text.Parsec (SourcePos)

type family InterpreterGroundType (ts :: Type) :: GroundTypeKind

type family InterpreterProvidedType (ts :: Type) :: forall k. k -> Type

type family InterpreterClosedEntityType (ts :: Type) :: Type -> Type

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
    = ValueBinding (TSSealedExpression ts)
                   (Maybe (TSPatternConstructor ts))
    | TypeBinding (BoundType ts)
    | SpecialFormBinding (SpecialForm ts (SourceInterpreter ts))

type DocInterpreterBinding ts = (Markdown, InterpreterBinding ts)

data Scope (ts :: Type) = MkScope
    { scopeBindings :: Map Name (DocInterpreterBinding ts)
    , scopeSubtypes :: HashMap Unique (SubypeConversionEntry (InterpreterGroundType ts))
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
    , icLoadModule :: ModuleName -> Interpreter ts (Maybe (Scope ts))
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

type Interpreter :: Type -> Type -> Type
newtype Interpreter ts a = MkInterpreter
    { unInterpreter :: ReaderT (InterpretContext ts) (StateT (InterpretState ts) InterpretResult) a
    } deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, MonadFix)

instance MonadThrow PinaforeError (Interpreter ts) where
    throw err = MkInterpreter $ throw err

instance MonadCatch PinaforeError (Interpreter ts) where
    catch (MkInterpreter ma) ema = MkInterpreter $ catch ma $ \e -> unInterpreter $ ema e

instance MonadThrow ErrorMessage (Interpreter ts) where
    throw = throwErrorMessage

instance Semigroup a => Semigroup (Interpreter ts a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Interpreter ts a) where
    mappend = (<>)
    mempty = pure mempty

runInterpreter ::
       (ModuleName -> Interpreter ts (Maybe (Scope ts))) -> SpecialVals ts -> Interpreter ts a -> InterpretResult a
runInterpreter icLoadModule icSpecialVals qa = let
    icScope = mempty
    icModulePath = []
    in evalStateT (runReaderT (unInterpreter qa) $ MkInterpretContext {..}) emptyInterpretState

liftInterpreter :: InterpretResult a -> Interpreter ts a
liftInterpreter ra = MkInterpreter $ lift $ lift ra

interpreterScope :: Interpreter ts (Scope ts)
interpreterScope = MkInterpreter $ asks icScope

purifyExpression ::
       forall ts. (Show (TSName ts), AllWitnessConstraint Show (TSNegWitness ts))
    => TSSealedExpression ts
    -> SourceInterpreter ts (TSSealedExpression ts)
purifyExpression expr = do
    _ <- tsEval @ts expr
    return expr

purifyBinding ::
       (Show (TSName ts), AllWitnessConstraint Show (TSNegWitness ts))
    => InterpreterBinding ts
    -> SourceInterpreter ts (InterpreterBinding ts)
purifyBinding (ValueBinding expr mpatc) = do
    expr' <- purifyExpression expr
    return $ ValueBinding expr' mpatc
purifyBinding b = return b

exportScope ::
       forall ts. (Show (TSName ts), AllWitnessConstraint Show (TSNegWitness ts))
    => [Name]
    -> SourceInterpreter ts (Scope ts)
exportScope names = do
    MkScope bindings subtypes <- spScope
    ees <-
        for names $ \name ->
            case lookup name bindings of
                Just (doc, b) -> do
                    b' <- purifyBinding b
                    return $ Right (name, (doc, b'))
                Nothing -> return $ Left name
    let (badnames, goodbinds) = partitionEithers ees
    case badnames of
        [] -> return $ MkScope (mapFromList goodbinds) subtypes
        (n:nn) -> throw $ LookupNamesUnknownError $ n :| nn

pLocalScope :: (Scope ts -> Scope ts) -> Interpreter ts a -> Interpreter ts a
pLocalScope maptc (MkInterpreter ma) = MkInterpreter $ local (\ic -> ic {icScope = maptc $ icScope ic}) ma

importScope :: Scope ts -> Interpreter ts a -> Interpreter ts a
importScope newscope = pLocalScope $ \oldscope -> newscope <> oldscope

getCycle :: ModuleName -> [ModuleName] -> Maybe (NonEmpty ModuleName)
getCycle _ [] = Nothing
getCycle mn (n:nn)
    | mn == n = Just $ n :| nn
getCycle mn (_:nn) = getCycle mn nn

loadModuleInScope :: forall ts. ModuleName -> Interpreter ts (Maybe (Scope ts))
loadModuleInScope mname = do
    oldic <- MkInterpreter ask
    let
        newic :: InterpretContext ts
        newic = oldic {icScope = mempty, icModulePath = icModulePath oldic <> [mname]}
    MkInterpreter $ lift $ runReaderT (unInterpreter (icLoadModule newic mname)) newic

getModule :: ModuleName -> SourceInterpreter ts (Scope ts)
getModule mname = do
    istate <- liftSourcePos $ MkInterpreter $ lift get
    let oldmodules = isModules istate
    case lookup mname oldmodules of
        Just m -> return m
        Nothing -> do
            mpath <- liftSourcePos $ MkInterpreter $ asks icModulePath
            case getCycle mname mpath of
                Just mnames -> throw $ ModuleCycleError mnames
                Nothing -> do
                    mm <- liftSourcePos $ loadModuleInScope mname
                    case mm of
                        Just m -> do
                            liftSourcePos $ MkInterpreter $ lift $ put istate {isModules = insertMap mname m oldmodules}
                            return m
                        Nothing -> throw $ ModuleNotFoundError mname

importModule :: SourcePos -> ModuleName -> MFunction (Interpreter ts) (Interpreter ts)
importModule spos mname ma = do
    newscope <- runSourcePos spos $ getModule mname
    importScope newscope ma

newtype SourceInterpreter ts a = MkSourceInterpreter
    { unSourceInterpreter :: ReaderT SourcePos (Interpreter ts) a
    } deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, MonadFix)

askSourcePos :: SourceInterpreter ts SourcePos
askSourcePos = MkSourceInterpreter ask

localSourcePos :: SourcePos -> SourceInterpreter ts a -> SourceInterpreter ts a
localSourcePos spos (MkSourceInterpreter ma) = MkSourceInterpreter $ local (\_ -> spos) ma

runSourcePos :: SourcePos -> SourceInterpreter ts a -> Interpreter ts a
runSourcePos spos (MkSourceInterpreter ma) = runReaderT ma spos

liftSourcePos :: Interpreter ts a -> SourceInterpreter ts a
liftSourcePos ma = MkSourceInterpreter $ lift ma

remonadSourcePos :: (forall a. Interpreter p1 a -> Interpreter p2 a) -> SourceInterpreter p1 b -> SourceInterpreter p2 b
remonadSourcePos mm (MkSourceInterpreter mb) = MkSourceInterpreter $ remonad mm mb

mapSourcePos ::
       forall ts a b.
       SourcePos
    -> (SourceInterpreter ts a -> SourceInterpreter ts b)
    -> Interpreter ts a
    -> Interpreter ts b
mapSourcePos spos f ca = runSourcePos spos $ f $ liftSourcePos ca

spScope :: SourceInterpreter ts (Scope ts)
spScope = liftSourcePos interpreterScope

instance MonadThrow ExpressionError (SourceInterpreter ts) where
    throw err = throw $ ExpressionErrorError err

instance MonadThrow PinaforeError (SourceInterpreter ts) where
    throw err = MkSourceInterpreter $ throw err

instance MonadCatch PinaforeError (SourceInterpreter ts) where
    catch (MkSourceInterpreter ma) ema = MkSourceInterpreter $ catch ma $ \e -> unSourceInterpreter $ ema e

instance MonadThrow ErrorMessage (SourceInterpreter ts) where
    throw err = MkSourceInterpreter $ throw err

instance MonadThrow ErrorType (SourceInterpreter ts) where
    throw err =
        MkSourceInterpreter $ do
            spos <- ask
            throwErrorType spos err

withNewBinding :: Name -> DocInterpreterBinding ts -> Interpreter ts a -> Interpreter ts a
withNewBinding name db = pLocalScope $ \tc -> tc {scopeBindings = insertMapLazy name db $ scopeBindings tc}

bindingsScope :: Map Name (DocInterpreterBinding ts) -> Scope ts
bindingsScope bb = mempty {scopeBindings = bb}

getSubtypesScope :: [SubypeConversionEntry (InterpreterGroundType ts)] -> IO (Scope ts)
getSubtypesScope newscs = do
    pairs <-
        for newscs $ \newsc -> do
            key <- newUnique
            return (key, newsc)
    return $ mempty {scopeSubtypes = mapFromList pairs}

withNewBindings :: Map Name (DocInterpreterBinding ts) -> Interpreter ts a -> Interpreter ts a
withNewBindings bb = importScope $ bindingsScope bb

withRemovedBindings :: [Name] -> Interpreter ts a -> Interpreter ts a
withRemovedBindings nn = pLocalScope $ \tc -> tc {scopeBindings = deletesMap nn $ scopeBindings tc}

lookupDocBinding :: ReferenceName -> SourceInterpreter ts (Maybe (DocInterpreterBinding ts))
lookupDocBinding (UnqualifiedReferenceName name) = do
    (scopeBindings -> names) <- spScope
    return $ lookup name names
lookupDocBinding (QualifiedReferenceName mname name) = do
    scope <- getModule mname
    return $ lookup name $ scopeBindings scope

lookupBinding :: ReferenceName -> SourceInterpreter ts (Maybe (InterpreterBinding ts))
lookupBinding rname = fmap (fmap snd) $ lookupDocBinding rname

getSpecialVals :: SourceInterpreter ts (SpecialVals ts)
getSpecialVals = liftSourcePos $ MkInterpreter $ asks icSpecialVals

lookupLetBinding :: ReferenceName -> SourceInterpreter ts (Maybe (TSSealedExpression ts))
lookupLetBinding name = do
    mb <- lookupBinding name
    case mb of
        Just (ValueBinding exp _) -> return $ Just exp
        _ -> return Nothing

withNewLetBindings :: Map Name (Markdown, TSSealedExpression ts) -> Interpreter ts a -> Interpreter ts a
withNewLetBindings bb = withNewBindings $ fmap (\(doc, exp) -> (doc, ValueBinding exp Nothing)) bb

lookupSpecialForm :: ReferenceName -> SourceInterpreter ts (SpecialForm ts (SourceInterpreter ts))
lookupSpecialForm name = do
    mb <- lookupBinding name
    case mb of
        Just (SpecialFormBinding sf) -> return sf
        _ -> throw $ LookupSpecialFormUnknownError name

lookupBoundTypeM :: ReferenceName -> SourceInterpreter ts (Maybe (BoundType ts))
lookupBoundTypeM name = do
    mb <- lookupBinding name
    return $
        case mb of
            Just (TypeBinding t) -> Just t
            _ -> Nothing

lookupBoundType :: ReferenceName -> SourceInterpreter ts (BoundType ts)
lookupBoundType name = do
    mnt <- lookupBoundTypeM name
    case mnt of
        Just nt -> return nt
        Nothing -> throw $ LookupTypeUnknownError name

lookupPatternConstructorM :: ReferenceName -> SourceInterpreter ts (Maybe (TSPatternConstructor ts))
lookupPatternConstructorM name = do
    mb <- lookupBinding name
    return $
        case mb of
            Just (ValueBinding _ (Just pc)) -> Just pc
            _ -> Nothing

lookupPatternConstructor :: ReferenceName -> SourceInterpreter ts (TSPatternConstructor ts)
lookupPatternConstructor name = do
    ma <- lookupPatternConstructorM name
    case ma of
        Just a -> return a
        Nothing -> throw $ LookupConstructorUnknownError name

newTypeID :: Interpreter ts TypeID
newTypeID =
    MkInterpreter $
    lift $ do
        istate <- get
        let tid = isTypeID istate
        put $ istate {isTypeID = succTypeID tid}
        return tid

registerType :: SourcePos -> Name -> Markdown -> BoundType ts -> WMFunction (Interpreter ts) (Interpreter ts)
registerType spos name doc t =
    MkWMFunction $ \mta ->
        runSourcePos spos $ do
            mnt <- lookupBoundTypeM $ UnqualifiedReferenceName name
            case mnt of
                Just _ -> throw $ DeclareTypeDuplicateError name
                Nothing -> liftSourcePos $ withNewBinding name (doc, TypeBinding t) mta

data TypeBox ts x =
    forall t. MkTypeBox Name
                        (t -> BoundType ts)
                        (Interpreter ts (t, x))

typeBoxToFixBox :: (SourcePos, Markdown, TypeBox ts x) -> FixBox (WriterT [x] (Interpreter ts))
typeBoxToFixBox (spos, doc, MkTypeBox name ttype mtx) =
    mkFixBox (\t -> liftWMFunction $ registerType spos name doc $ ttype t) $ do
        (t, x) <- lift mtx
        tell [x]
        return t

registerTypeNames ::
       [(SourcePos, Markdown, TypeBox ts x)] -> Interpreter ts (WMFunction (Interpreter ts) (Interpreter ts), [x])
registerTypeNames tboxes = do
    (sc, xx) <- runWriterT $ boxFix (fmap typeBoxToFixBox tboxes) $ lift interpreterScope
    return (MkWMFunction $ pLocalScope (\_ -> sc), xx)

withNewPatternConstructor ::
       Name
    -> Markdown
    -> TSSealedExpression ts
    -> TSPatternConstructor ts
    -> SourceInterpreter ts (WMFunction (Interpreter ts) (Interpreter ts))
withNewPatternConstructor name doc exp pc = do
    ma <- lookupPatternConstructorM $ UnqualifiedReferenceName name
    case ma of
        Just _ -> throw $ DeclareConstructorDuplicateError name
        Nothing -> return $ MkWMFunction $ withNewBinding name $ (doc, ValueBinding exp $ Just pc)

withSubtypeConversions :: [SubypeConversionEntry (InterpreterGroundType ts)] -> Interpreter ts a -> Interpreter ts a
withSubtypeConversions newscs ma = do
    newscope <- liftIO $ getSubtypesScope newscs
    importScope newscope ma

getSubtypeConversions :: Interpreter ts [SubypeConversionEntry (InterpreterGroundType ts)]
getSubtypeConversions = fmap (fmap snd . mapToList . scopeSubtypes) interpreterScope
