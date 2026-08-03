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
    )
where

import Changes.Core.Import
import Changes.Core.Resource.SingleRunner

newtype ResourceRunner (tt :: [Type])
    = MkResourceRunner (ListType SingleRunner tt)

nilResourceRunner :: ResourceRunner '[]
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
    ResourceRunner tta ->
    ResourceRunner ttb ->
    (forall ttab. ResourceRunner ttab -> (ListProduct ttab -> ListProduct tta) -> (ListProduct ttab -> ListProduct ttb) -> r) ->
    r
combineResourceRunners (MkResourceRunner la) (MkResourceRunner lb) call =
    combineLSR la lb $ \lab -> call (MkResourceRunner lab)

singleResourceRunner :: SingleRunner t -> ResourceRunner '[t]
singleResourceRunner sr = MkResourceRunner $ ConsListType sr NilListType

mkResourceRunner ::
    forall t.
    IOWitness t ->
    With IO t ->
    ResourceRunner '[t]
mkResourceRunner iow run = singleResourceRunner $ mkSingleRunner iow run

newResourceRunner ::
    forall t.
    With IO t ->
    IO (ResourceRunner '[t])
newResourceRunner run = do
    iow <- newIOWitness
    return $ mkResourceRunner iow run

stateResourceRunner :: s -> IO (ResourceRunner '[MVar s])
stateResourceRunner s = do
    var <- newMVar s
    iow <- newIOWitness
    return $ mvarResourceRunner iow var

mvarResourceRunner :: IOWitness (MVar s) -> MVar s -> ResourceRunner '[MVar s]
mvarResourceRunner iow var =
    mkResourceRunner iow $ \call -> mVarRunStateT var $ liftWithMVarStateT call

discardingStateResourceRunner :: IOWitness (MVar s) -> s -> ResourceRunner '[MVar s]
discardingStateResourceRunner iow s =
    mkResourceRunner iow $ \call -> discardingStateTUnlift s $ liftWithMVarStateT call

runResourceStateT :: StateT s IO --> ReaderT (ListProduct '[MVar s]) IO
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

runLSR ::
    forall tt m r.
    MonadUnliftIO m =>
    [Some SingleRunner] ->
    ListType SingleRunner tt ->
    (ListProduct tt -> m r) ->
    m r
runLSR _ NilListType call = call ()
runLSR rc (ConsListType (sr :: _ t) (lsr :: _ tt0)) call =
    runLSR rc lsr $ \ttr ->
        runSingleRunner rc sr $ \t -> call (t, ttr)

runResourceRunner ::
    forall tt r.
    ResourceContext ->
    ResourceRunner tt ->
    (ListProduct tt -> IO r) ->
    IO r
runResourceRunner (MkResourceContext rc) (MkResourceRunner rr) = runLSR rc rr

runLSRContext ::
    forall tt m r.
    MonadTunnelIO m =>
    [Some SingleRunner] ->
    ListType SingleRunner tt ->
    ([Some SingleRunner] -> ListProduct tt -> m r) ->
    m r
runLSRContext rc NilListType call = call rc ()
runLSRContext rc (ConsListType (sr :: _ t) (lsr :: _ tt0)) call =
    runLSRContext rc lsr $ \rc' ttr ->
        runSingleRunnerContext rc' sr $ \rc'' t -> call rc'' (t, ttr)

runResourceRunnerContext ::
    forall tt r.
    ResourceContext ->
    ResourceRunner tt ->
    (ResourceContext -> ListProduct tt -> IO r) ->
    IO r
runResourceRunnerContext (MkResourceContext rc) (MkResourceRunner rr) call =
    runLSRContext rc rr $ \rc' -> call (MkResourceContext rc')

exclusiveResourceRunner ::
    forall tt.
    ResourceContext ->
    ResourceRunner tt ->
    LifecycleT IO IO (ResourceRunner '[ListProduct tt])
exclusiveResourceRunner rc rr = do
    iow <- liftIO newIOWitness
    lifecycleWith $ \call ->
        runResourceRunnerContext rc rr $ \_ tt -> call $ mkResourceRunner iow $ \use -> use tt
