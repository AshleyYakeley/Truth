module Changes.Core.Resource.ResourceRunner
    ( ResourceRunner
    , nilResourceRunner
    , combineResourceRunners
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

data ResourceRunner (t :: Type) where
    MkResourceRunner :: forall (tt :: [Type]). ListType SingleRunner tt -> ResourceRunner (ListProduct tt)
    DependentResourceRunner :: ResourceRunner a -> (a -> IO (ResourceRunner b)) -> ResourceRunner b

instance Functor ResourceRunner where
    fmap _ (MkResourceRunner _) = error "NYI"
    fmap ab (DependentResourceRunner rx xira) = DependentResourceRunner rx $ (fmap $ fmap $ fmap ab) xira

nilResourceRunner :: ResourceRunner ()
nilResourceRunner = MkResourceRunner NilListType

emptyListProductFunction :: ListProduct tt -> ListProduct '[]
emptyListProductFunction _ = ()

consListProductFunction ::
    (ListProduct ttb -> ListProduct tta) ->
    ListProduct (t ': ttb) ->
    ListProduct (t ': tta)
consListProductFunction f (t, tt) = (t, f tt)

tailListProductFunction :: ListProduct (t ': tt) -> ListProduct tt
tailListProductFunction = snd

combineLSR ::
    ListType SingleRunner tta ->
    ListType SingleRunner ttb ->
    (forall ttab. ListType SingleRunner ttab -> (ListProduct ttab -> ListProduct tta) -> (ListProduct ttab -> ListProduct ttb) -> r) ->
    r
combineLSR NilListType rb call = call rb emptyListProductFunction id
combineLSR ra NilListType call = call ra id emptyListProductFunction
combineLSR au1@(ConsListType u1 uu1) au2@(ConsListType u2 uu2) call = case testCompare u1 u2 of
    WEQ ->
        combineLSR uu1 uu2 $ \uu12 tf1 tf2 ->
            call
                (ConsListType u1 uu12)
                (consListProductFunction tf1)
                (consListProductFunction tf2)
    WLT ->
        combineLSR uu1 au2 $ \uu12 tf1 tf2 ->
            call
                (ConsListType u1 uu12)
                (consListProductFunction tf1)
                (tf2 . tailListProductFunction)
    WGT ->
        combineLSR au1 uu2 $ \uu12 tf1 tf2 ->
            call
                (ConsListType u2 uu12)
                (tf1 . tailListProductFunction)
                (consListProductFunction tf2)

combineResourceRunners ::
    ResourceRunner ta ->
    ResourceRunner tb ->
    (forall tab. ResourceRunner tab -> (tab -> ta) -> (tab -> tb) -> r) ->
    r
combineResourceRunners (MkResourceRunner la) (MkResourceRunner lb) call =
    combineLSR la lb $ \lab -> call (MkResourceRunner lab)
combineResourceRunners _ _ _ = error "NYI"

singleResourceRunner :: SingleRunner t -> ResourceRunner (t, ())
singleResourceRunner sr = MkResourceRunner $ ConsListType sr NilListType

dependentResourceRunner :: ResourceRunner a -> (a -> IO (ResourceRunner b)) -> ResourceRunner b
dependentResourceRunner = DependentResourceRunner

mkResourceRunner ::
    forall t.
    IOWitness t ->
    With IO t ->
    ResourceRunner (t, ())
mkResourceRunner iow run = singleResourceRunner $ mkSingleRunner iow run

newResourceRunner ::
    forall t.
    With IO t ->
    IO (ResourceRunner (t, ()))
newResourceRunner run = do
    iow <- newIOWitness
    return $ mkResourceRunner iow run

stateResourceRunner :: s -> IO (ResourceRunner (MVar s, ()))
stateResourceRunner s = do
    var <- newMVar s
    iow <- newIOWitness
    return $ mvarResourceRunner iow var

mvarResourceRunner :: IOWitness (MVar s) -> MVar s -> ResourceRunner (MVar s, ())
mvarResourceRunner iow var =
    mkResourceRunner iow $ \call -> mVarRunStateT var $ liftWithMVarStateT call

discardingStateResourceRunner :: IOWitness (MVar s) -> s -> ResourceRunner (MVar s, ())
discardingStateResourceRunner iow s =
    mkResourceRunner iow $ \call -> discardingStateTUnlift s $ liftWithMVarStateT call

runResourceStateT :: StateT s IO --> ReaderT (MVar s, ()) IO
runResourceStateT ma = do
    stateVar <- asks fst
    liftIO $ mVarRunStateT stateVar ma

newtype ResourceContext
    = MkResourceContext [Some SingleRunner]

emptyResourceContext :: ResourceContext
emptyResourceContext = MkResourceContext []

-- | for debugging
resourceContextSize :: ResourceContext -> Int
resourceContextSize (MkResourceContext rc) = length rc

runLSRContext ::
    forall tt r.
    [Some SingleRunner] ->
    ListType SingleRunner tt ->
    ([Some SingleRunner] -> ListProduct tt -> IO r) ->
    IO r
runLSRContext rc NilListType call = call rc ()
runLSRContext rc (ConsListType (sr :: _ t) (lsr :: _ tt0)) call =
    runLSRContext rc lsr $ \rc' ttr ->
        runSingleRunner rc' sr $ \rc'' t -> call rc'' (t, ttr)

runResourceRunnerContext ::
    forall t r.
    ResourceContext ->
    ResourceRunner t ->
    (ResourceContext -> t -> IO r) ->
    IO r
runResourceRunnerContext (MkResourceContext rc) (MkResourceRunner rr) call =
    runLSRContext rc rr $ \rc' -> call (MkResourceContext rc')
runResourceRunnerContext rc (DependentResourceRunner ra arb) call =
    runResourceRunnerContext rc ra $ \rc' a -> do
        rb <- arb a
        runResourceRunnerContext rc' rb call

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
    LifecycleT IO IO (ResourceRunner (t, ()))
exclusiveResourceRunner rc rr = do
    iow <- liftIO newIOWitness
    lifecycleWith $ \call ->
        runResourceRunnerContext rc rr $ \_ tt -> call $ mkResourceRunner iow $ \use -> use tt
