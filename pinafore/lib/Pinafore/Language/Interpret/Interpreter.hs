module Pinafore.Language.Interpret.Interpreter
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
    , convertFailure
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
import Pinafore.Language.Type.DynamicEntity
import Pinafore.Language.Type.Identified
import Pinafore.Language.Type.Show
import Shapes
import Text.Parsec (SourcePos)

type family InterpreterGroundType (ts :: Type) :: GroundTypeKind

type family InterpreterProvidedType (ts :: Type) :: forall k. k -> Type

type family InterpreterClosedEntityType (ts :: Type) :: Type -> Type

data BoundType (ts :: Type) where
    SimpleBoundType
        :: forall (ts :: Type) (dv :: DolanVariance) (t :: DolanVarianceKind dv).
           DolanVarianceType dv
        -> DolanVarianceMap dv t
        -> ListTypeExprShow dv
        -> InterpreterProvidedType ts t
        -> BoundType ts
    OpenEntityBoundType :: TypeID -> BoundType ts
    ClosedEntityBoundType
        :: forall (ts :: Type) (tid :: BigNat).
           TypeIDType tid
        -> InterpreterClosedEntityType ts (Identified tid)
        -> BoundType ts
    DynamicEntityBoundType :: forall (ts :: Type). DynamicEntityType -> BoundType ts

newtype SpecialVals (ts :: Type) = MkSpecialVals
    { specialEvaluate :: forall t. TSPosWitness ts t -> Text -> PinaforeAction (Either Text t)
        -- ^ in Action because this can do things like import files
    }

data InterpreterBinding (ts :: Type)
    = ValueBinding (TSSealedExpression ts)
                   (Maybe (TSPatternConstructor ts))
    | TypeBinding (BoundType ts)
    | SpecialFormBinding (SpecialForm ts (SourceInterpreter ts))

data Scope (ts :: Type) = MkScope
    { scopeBindings :: Map Name (InterpreterBinding ts)
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
                Just b -> do
                    b' <- purifyBinding b
                    return $ Right (name, b')
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

importModule :: ModuleName -> SourceInterpreter ts (WMFunction (Interpreter ts) (Interpreter ts))
importModule mname = do
    newscope <- getModule mname
    return $ MkWMFunction $ importScope newscope

newtype SourceInterpreter ts a =
    MkSourceInterpreter (ReaderT SourcePos (Interpreter ts) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, MonadFix)

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

instance MonadThrow ErrorMessage (SourceInterpreter ts) where
    throw err = MkSourceInterpreter $ throw err

instance MonadThrow ErrorType (SourceInterpreter ts) where
    throw err =
        MkSourceInterpreter $ do
            spos <- ask
            throw $ MkErrorMessage spos err

convertFailure :: Text -> Text -> SourceInterpreter ts a
convertFailure ta tb = throw $ TypeConvertError ta tb

withNewBinding :: Name -> InterpreterBinding ts -> Interpreter ts a -> Interpreter ts a
withNewBinding name b = pLocalScope $ \tc -> tc {scopeBindings = insertMapLazy name b $ scopeBindings tc}

bindingsScope :: Map Name (InterpreterBinding ts) -> Scope ts
bindingsScope bb = mempty {scopeBindings = bb}

getSubtypesScope :: MonadIO m => [SubypeConversionEntry (InterpreterGroundType ts)] -> m (Scope ts)
getSubtypesScope newscs = do
    pairs <-
        liftIO $
        for newscs $ \newsc -> do
            key <- newUnique
            return (key, newsc)
    return $ mempty {scopeSubtypes = mapFromList pairs}

withNewBindings :: Map Name (InterpreterBinding ts) -> Interpreter ts a -> Interpreter ts a
withNewBindings bb = importScope $ bindingsScope bb

withRemovedBindings :: [Name] -> Interpreter ts a -> Interpreter ts a
withRemovedBindings nn = pLocalScope $ \tc -> tc {scopeBindings = deletesMap nn $ scopeBindings tc}

lookupBinding :: Name -> SourceInterpreter ts (Maybe (InterpreterBinding ts))
lookupBinding name = do
    (scopeBindings -> names) <- spScope
    return $ lookup name names

getSpecialVals :: SourceInterpreter ts (SpecialVals ts)
getSpecialVals = liftSourcePos $ MkInterpreter $ asks icSpecialVals

lookupLetBinding :: Name -> SourceInterpreter ts (Maybe (TSSealedExpression ts))
lookupLetBinding name = do
    mb <- lookupBinding name
    case mb of
        Just (ValueBinding exp _) -> return $ Just exp
        _ -> return Nothing

withNewLetBindings :: Map Name (TSSealedExpression ts) -> Interpreter ts a -> Interpreter ts a
withNewLetBindings bb = withNewBindings $ fmap (\exp -> ValueBinding exp Nothing) bb

lookupSpecialForm :: Name -> SourceInterpreter ts (SpecialForm ts (SourceInterpreter ts))
lookupSpecialForm name = do
    mb <- lookupBinding name
    case mb of
        Just (SpecialFormBinding sf) -> return sf
        _ -> throw $ LookupSpecialFormUnknownError name

lookupBoundTypeM :: Name -> SourceInterpreter ts (Maybe (BoundType ts))
lookupBoundTypeM name = do
    mb <- lookupBinding name
    return $
        case mb of
            Just (TypeBinding t) -> Just t
            _ -> Nothing

lookupBoundType :: Name -> SourceInterpreter ts (BoundType ts)
lookupBoundType name = do
    mnt <- lookupBoundTypeM name
    case mnt of
        Just nt -> return nt
        Nothing -> throw $ LookupTypeUnknownError name

lookupPatternConstructorM :: Name -> SourceInterpreter ts (Maybe (TSPatternConstructor ts))
lookupPatternConstructorM name = do
    mb <- lookupBinding name
    return $
        case mb of
            Just (ValueBinding _ (Just pc)) -> Just pc
            _ -> Nothing

lookupPatternConstructor :: Name -> SourceInterpreter ts (TSPatternConstructor ts)
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

registerType :: Name -> BoundType ts -> WMFunction (SourceInterpreter ts) (SourceInterpreter ts)
registerType name t =
    MkWMFunction $ \mta -> do
        mnt <- lookupBoundTypeM name
        case mnt of
            Just _ -> throw $ DeclareTypeDuplicateError name
            Nothing -> remonadSourcePos (withNewBinding name $ TypeBinding t) mta

data TypeBox ts x =
    forall t. MkTypeBox Name
                        (t -> BoundType ts)
                        (SourceInterpreter ts (t, x))

typeBoxToFixBox :: TypeBox ts x -> FixBox (WriterT [x] (SourceInterpreter ts))
typeBoxToFixBox (MkTypeBox name ttype mtx) =
    mkFixBox (\t -> liftWMFunction $ registerType name $ ttype t) $ do
        (t, x) <- lift mtx
        tell [x]
        return t

registerTypeNames :: [TypeBox ts x] -> SourceInterpreter ts (WMFunction (Interpreter ts) (Interpreter ts), [x])
registerTypeNames tboxes = do
    (sc, xx) <- runWriterT $ boxFix (fmap typeBoxToFixBox tboxes) $ lift spScope
    return (MkWMFunction $ pLocalScope (\_ -> sc), xx)

withNewPatternConstructor ::
       Name
    -> TSSealedExpression ts
    -> TSPatternConstructor ts
    -> SourceInterpreter ts (WMFunction (Interpreter ts) (Interpreter ts))
withNewPatternConstructor name exp pc = do
    ma <- lookupPatternConstructorM name
    case ma of
        Just _ -> throw $ DeclareConstructorDuplicateError name
        Nothing -> return $ MkWMFunction $ withNewBinding name $ ValueBinding exp $ Just pc

withSubtypeConversions :: [SubypeConversionEntry (InterpreterGroundType ts)] -> Interpreter ts a -> Interpreter ts a
withSubtypeConversions newscs ma = do
    newscope <- getSubtypesScope newscs
    importScope newscope ma

getSubtypeConversions :: Interpreter ts [SubypeConversionEntry (InterpreterGroundType ts)]
getSubtypeConversions = fmap (fmap snd . mapToList . scopeSubtypes) interpreterScope
