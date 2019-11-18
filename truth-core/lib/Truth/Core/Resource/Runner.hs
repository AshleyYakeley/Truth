module Truth.Core.Resource.Runner
    ( TransStackRunner
    , cmEmpty
    , cmAppend
    , transStackRunnerUnliftAllDict
    , runTransStackRunner
    , runMonoTransStackRunner
    , singleTransStackRunner
    , mVarTransStackRunner
    , stateTransStackRunner
    , discardingStateTransStackRunner
    , exclusiveTransStackRunner
    , discardingTransStackRunner
    ) where

import Truth.Core.Import

data TransStackRunner (tt :: [TransKind]) where
    MkTransStackRunner
        :: MonadTransStackUnliftAll tt
        => (forall m. MonadUnliftIO m => MFunction (ApplyStack tt m) m)
        -> TransStackRunner tt

transStackRunnerUnliftAllDict :: TransStackRunner tt -> Dict (MonadTransStackUnliftAll tt)
transStackRunnerUnliftAllDict (MkTransStackRunner _) = Dict

runTransStackRunner ::
       forall tt r.
       TransStackRunner tt
    -> (MonadTransStackUnliftAll tt => (forall m. MonadUnliftIO m => MFunction (ApplyStack tt m) m) -> r)
    -> r
runTransStackRunner (MkTransStackRunner run) call = call run

runMonoTransStackRunner ::
       forall m tt r. MonadUnliftIO m
    => TransStackRunner tt
    -> ((MonadTransStackUnliftAll tt, MonadUnliftIO (ApplyStack tt m)) => (MFunction (ApplyStack tt m) m) -> r)
    -> r
runMonoTransStackRunner tr call =
    runTransStackRunner tr $
    case transStackDict @MonadUnliftIO @tt @m of
        Dict -> call

-- | Opens up the runner and keeps it open until the LifeCycle closes.
exclusiveTransStackRunner :: forall tt. TransStackRunner tt -> LifeCycleIO (TransStackRunner tt)
exclusiveTransStackRunner tr =
    lifeCycleWith $ \call ->
        runMonoTransStackRunner tr $ \run ->
            run $
            unStackT $ liftWithUnliftAll $ \unlift -> call $ MkTransStackRunner $ \ama -> unlift $ MkStackT @tt ama

discardingTransStackRunner :: forall tt. TransStackRunner tt -> TransStackRunner tt
discardingTransStackRunner (MkTransStackRunner run) = let
    run' ::
           forall m. MonadUnliftIO m
        => MFunction (ApplyStack tt m) m
    run' tmr = do
        MkWUnliftAll du <- run $ unStackT @tt @m getDiscardingUnliftAll
        du $ MkStackT tmr
    in MkTransStackRunner run'

singleTransStackRunner ::
       forall t. MonadTransUnliftAll t
    => UnliftAll MonadUnliftIO t
    -> TransStackRunner '[ t]
singleTransStackRunner = MkTransStackRunner

mVarTransStackRunner :: MVar s -> TransStackRunner '[ StateT s]
mVarTransStackRunner var = singleTransStackRunner $ mVarRun var

-- | maintains state (in an MVar) between runs, also enforcing thread mutual exclusion
stateTransStackRunner :: s -> IO (TransStackRunner '[ StateT s])
stateTransStackRunner s = do
    var <- newMVar s
    return $ mVarTransStackRunner var

-- | uses the same initial state for each run, final state is discarded at the end of the run
discardingStateTransStackRunner :: forall s. s -> TransStackRunner '[ StateT s]
discardingStateTransStackRunner s = MkTransStackRunner $ stateDiscardingUntrans s

cmEmpty :: TransStackRunner '[]
cmEmpty = MkTransStackRunner id

cmAppend :: forall tt1 tt2. TransStackRunner tt1 -> TransStackRunner tt2 -> TransStackRunner (Concat tt1 tt2)
cmAppend (MkTransStackRunner mf1) (MkTransStackRunner mf2) = let
    mf12 ::
           forall m. MonadUnliftIO m
        => MFunction (ApplyStack (Concat tt1 tt2) m) m
    mf12 =
        case transStackConcatRefl @tt1 @tt2 @m of
            Refl ->
                case transStackDict @MonadUnliftIO @tt2 @m of
                    Dict -> mf2 . mf1
    in case concatMonadTransStackUnliftAllDict @tt1 @tt2 of
           Dict -> MkTransStackRunner mf12
