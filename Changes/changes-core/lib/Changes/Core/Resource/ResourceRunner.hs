module Changes.Core.Resource.ResourceRunner
    ( ResourceRunner
    , mkResourceRunner
    , newResourceRunner
    , stateResourceRunner
    , mvarResourceRunner
    , discardingStateResourceRunner
    , runResourceStateT
    , exclusiveResourceRunner
    , ResourceContext
    , emptyResourceContext
    , resourceContextSize
    , runResourceRunner
    , runResourceRunnerContext
    , dependentResourceRunner
    )
where

import Changes.Core.Import
import Changes.Core.Resource.SingleRunner

combineFreeApplicative :: forall w a b c. TestOrder w => (a -> b -> c) -> FreeApplicative w a -> FreeApplicative w b -> FreeApplicative w c
combineFreeApplicative abc (PureFreeApplicative a) fb = fmap (\b -> abc a b) fb
combineFreeApplicative abc fa (PureFreeApplicative b) = fmap (\a -> abc a b) fa
combineFreeApplicative abc fa@(ApFreeApplicative wa fa1) fb@(ApFreeApplicative wb fb1) = case testCompare wa wb of
    WEQ -> ApFreeApplicative wa $ combineFreeApplicative (\xa xb x -> abc (xa x) (xb x)) fa1 fb1
    WLT -> ApFreeApplicative wa $ combineFreeApplicative (\xa b x -> abc (xa x) b) fa1 fb
    WGT -> ApFreeApplicative wb $ combineFreeApplicative (\a xb x -> abc a (xb x)) fa fb1

data ResourceRunner (t :: Type) where
    SimpleResourceRunner :: FreeApplicative SingleRunner a -> ResourceRunner a
    DependentResourceRunner :: FreeApplicative SingleRunner (IO (ResourceRunner a)) -> ResourceRunner a

instance Functor ResourceRunner where
    fmap ab (SimpleResourceRunner ast) = SimpleResourceRunner $ fmap ab ast
    fmap ab (DependentResourceRunner fira) = DependentResourceRunner $ fmap (fmap $ fmap ab) fira

instance Applicative ResourceRunner where
    pure a = SimpleResourceRunner $ pure a
    liftA2 abc (SimpleResourceRunner fa) (SimpleResourceRunner fb) = SimpleResourceRunner $ combineFreeApplicative abc fa fb
    liftA2 abc (SimpleResourceRunner fa) (DependentResourceRunner fiorb) =
        DependentResourceRunner $ combineFreeApplicative (\a iorb -> fmap (fmap $ \b -> abc a b) iorb) fa fiorb
    liftA2 abc (DependentResourceRunner fiora) (SimpleResourceRunner fb) =
        DependentResourceRunner $ combineFreeApplicative (\iora b -> fmap (fmap $ \a -> abc a b) iora) fiora fb
    liftA2 abc (DependentResourceRunner fiora) (DependentResourceRunner fiorb) =
        DependentResourceRunner $ combineFreeApplicative (liftA2 $ liftA2 abc) fiora fiorb

singleResourceRunner :: SingleRunner t -> ResourceRunner t
singleResourceRunner sr = SimpleResourceRunner $ toFree1 @_ @_ @Applicative sr

dependentResourceRunner :: ResourceRunner a -> (a -> IO (ResourceRunner b)) -> ResourceRunner b
dependentResourceRunner (SimpleResourceRunner fa) aiorb = DependentResourceRunner $ fmap aiorb fa
dependentResourceRunner (DependentResourceRunner fiora) aiorb =
    DependentResourceRunner $ fiora <&> \iora -> do
        ra <- iora
        pure $ dependentResourceRunner ra aiorb

mkResourceRunner ::
    forall t.
    IOWitness t ->
    With IO t ->
    ResourceRunner t
mkResourceRunner iow run = singleResourceRunner $ mkSingleRunner iow run

newResourceRunner ::
    forall t.
    With IO t ->
    IO (ResourceRunner t)
newResourceRunner run = do
    iow <- newIOWitness
    return $ mkResourceRunner iow run

stateResourceRunner :: s -> IO (ResourceRunner (MVar s))
stateResourceRunner s = do
    var <- newMVar s
    iow <- newIOWitness
    return $ mvarResourceRunner iow var

mvarResourceRunner :: IOWitness (MVar s) -> MVar s -> ResourceRunner (MVar s)
mvarResourceRunner iow var =
    mkResourceRunner iow $ \call -> mVarRunStateT var $ liftWithMVarStateT call

discardingStateResourceRunner :: IOWitness (MVar s) -> s -> ResourceRunner (MVar s)
discardingStateResourceRunner iow s =
    mkResourceRunner iow $ \call -> discardingStateTUnlift s $ liftWithMVarStateT call

runResourceStateT :: StateT s IO --> ReaderT (MVar s) IO
runResourceStateT ma = do
    stateVar <- ask
    liftIO $ mVarRunStateT stateVar ma

newtype ResourceContext
    = MkResourceContext [Some SingleRunner]

emptyResourceContext :: ResourceContext
emptyResourceContext = MkResourceContext []

-- | for debugging
resourceContextSize :: ResourceContext -> Int
resourceContextSize (MkResourceContext rc) = length rc

runSimpleContext ::
    [Some SingleRunner] ->
    FreeApplicative SingleRunner t ->
    ([Some SingleRunner] -> t -> IO r) ->
    IO r
runSimpleContext rc (PureFreeApplicative a) call = call rc a
runSimpleContext rc (ApFreeApplicative a fa) call =
    runSimpleContext rc fa $ \rc' ttr ->
        runSingleRunner rc' a $ \rc'' t -> call rc'' $ ttr t

runResourceRunnerContext ::
    forall t r.
    ResourceContext ->
    ResourceRunner t ->
    (ResourceContext -> t -> IO r) ->
    IO r
runResourceRunnerContext (MkResourceContext rc) (SimpleResourceRunner rr) call =
    runSimpleContext rc rr $ \rc' -> call (MkResourceContext rc')
runResourceRunnerContext (MkResourceContext rc) (DependentResourceRunner rra) call =
    runSimpleContext rc rra $ \rc' iora -> do
        ra <- iora
        runResourceRunnerContext (MkResourceContext rc') ra call

runResourceRunner ::
    forall t r.
    ResourceContext ->
    ResourceRunner t ->
    (t -> IO r) ->
    IO r
runResourceRunner rc runner call = runResourceRunnerContext rc runner $ \_ t -> call t

exclusiveResourceRunner ::
    forall t.
    ResourceContext ->
    ResourceRunner t ->
    LifecycleT IO IO (ResourceRunner t)
exclusiveResourceRunner rc rr = do
    iow <- liftIO newIOWitness
    lifecycleWith $ \call ->
        runResourceRunnerContext rc rr $ \_ tt -> call $ mkResourceRunner iow $ \use -> use tt
