module Truth.Core.Resource.ResourceRunner
    ( ResourceRunner
    , nilResourceRunner
    , combineIndependentResourceRunners
    , combineResourceRunners
    , resourceRunnerUnliftAllDict
    , runResourceRunner
    , runResourceRunnerWith
    , mkResourceRunner
    , newResourceRunner
    , stateResourceRunner
    , mvarResourceRunner
    , discardingStateResourceRunner
    , exclusiveResourceRunner
    , discardingResourceRunner
    ) where

import Truth.Core.Import
import Truth.Core.Resource.Function
import Truth.Core.Resource.SingleRunner

newtype ResourceRunner (tt :: [TransKind]) =
    MkResourceRunner (ListType SingleRunner tt)

nilResourceRunner :: ResourceRunner '[]
nilResourceRunner = MkResourceRunner NilListType

mapResourceRunner ::
       forall (ct :: TransKind -> Constraint) (tt :: [TransKind]). (forall t. MonadTransUnliftAll t => ct t)
    => ListType SingleRunner tt
    -> ListType (Compose Dict ct) tt
mapResourceRunner = mapListType $ singleRunnerComposeDict @ct

combineIndependentResourceRunners :: ResourceRunner tta -> ResourceRunner ttb -> ResourceRunner (Concat tta ttb)
combineIndependentResourceRunners (MkResourceRunner la) (MkResourceRunner lb) = MkResourceRunner $ concatListType la lb

combineLSR ::
       ListType SingleRunner tta
    -> ListType SingleRunner ttb
    -> (forall ttab. ListType SingleRunner ttab -> TransListFunction tta ttab -> TransListFunction ttb ttab -> r)
    -> r
combineLSR NilListType rb call =
    case lsrUnliftAllDict rb of
        Dict -> call rb emptyTransListFunction id
combineLSR ra NilListType call =
    case lsrUnliftAllDict ra of
        Dict -> call ra id emptyTransListFunction
combineLSR au1@(ConsListType u1 uu1) au2@(ConsListType u2 uu2) call =
    case singleRunnerUnliftAllDict u1 of
        Dict ->
            case singleRunnerUnliftAllDict u2 of
                Dict ->
                    case testOrder u1 u2 of
                        WEQ ->
                            combineLSR uu1 uu2 $ \uu12 tf1 tf2 ->
                                call
                                    (ConsListType u1 uu12)
                                    (consTransListFunction (mapResourceRunner uu1) (mapResourceRunner uu12) tf1)
                                    (consTransListFunction (mapResourceRunner uu2) (mapResourceRunner uu12) tf2)
                        WLT ->
                            combineLSR uu1 au2 $ \uu12 tf1 tf2 ->
                                case lsrUnliftAllDict uu12 of
                                    Dict ->
                                        call
                                            (ConsListType u1 uu12)
                                            (consTransListFunction (mapResourceRunner uu1) (mapResourceRunner uu12) tf1)
                                            (liftTransListFunction . tf2)
                        WGT ->
                            combineLSR au1 uu2 $ \uu12 tf1 tf2 ->
                                case lsrUnliftAllDict uu12 of
                                    Dict ->
                                        call
                                            (ConsListType u2 uu12)
                                            (liftTransListFunction . tf1)
                                            (consTransListFunction (mapResourceRunner uu2) (mapResourceRunner uu12) tf2)

combineResourceRunners ::
       ResourceRunner tta
    -> ResourceRunner ttb
    -> (forall ttab. ResourceRunner ttab -> TransListFunction tta ttab -> TransListFunction ttb ttab -> r)
    -> r
combineResourceRunners (MkResourceRunner la) (MkResourceRunner lb) call =
    combineLSR la lb $ \lab -> call (MkResourceRunner lab)

lsrUnliftAllDict :: ListType SingleRunner tt -> Dict (MonadTransStackUnliftAll tt)
lsrUnliftAllDict NilListType = Dict
lsrUnliftAllDict (ConsListType (singleRunnerUnliftAllDict -> Dict) (lsrUnliftAllDict -> Dict)) = Dict

resourceRunnerUnliftAllDict :: ResourceRunner tt -> Dict (MonadTransStackUnliftAll tt)
resourceRunnerUnliftAllDict (MkResourceRunner lsr) = lsrUnliftAllDict lsr

runLSR :: ListType SingleRunner tt -> (WStackUnliftAll tt, Dict (IsStack (MonadTransConstraint MonadUnliftIO) tt))
runLSR NilListType = (MkWStackUnliftAll id, Dict)
runLSR (ConsListType sr w) =
    case (singleRunnerUnliftAllDict sr, runLSR w) of
        (Dict, (runR, Dict)) -> (consWStackUnliftAll (MkWUnliftAll $ runSingleRunner sr) runR, Dict)

runResourceRunner :: ResourceRunner tt -> StackUnliftAll tt
runResourceRunner (MkResourceRunner lsr) = runWStackUnliftAll $ fst $ runLSR lsr

runResourceRunnerWith ::
       forall tt r.
       ResourceRunner tt
    -> ((MonadTransStackUnliftAll tt, MonadUnliftIO (ApplyStack tt IO)) => StackUnliftAll tt -> r)
    -> r
runResourceRunnerWith rr call =
    case resourceRunnerUnliftAllDict rr of
        Dict ->
            case transStackDict @MonadUnliftIO @tt @IO of
                Dict -> call $ runResourceRunner rr

singleResourceRunner :: SingleRunner t -> ResourceRunner '[ t]
singleResourceRunner sr = MkResourceRunner $ ConsListType sr NilListType

mkResourceRunner ::
       forall t. MonadTransUnliftAll t
    => IOWitness t
    -> UnliftAll MonadUnliftIO t
    -> ResourceRunner '[ t]
mkResourceRunner iow run = singleResourceRunner $ MkSingleRunner iow run

newResourceRunner ::
       forall t. MonadTransUnliftAll t
    => UnliftAll MonadUnliftIO t
    -> IO (ResourceRunner '[ t])
newResourceRunner run = do
    iow <- newIOWitness
    return $ mkResourceRunner iow run

stateResourceRunner :: s -> IO (ResourceRunner '[ StateT s])
stateResourceRunner s = do
    var <- newMVar s
    newResourceRunner $ mVarRun var

mvarResourceRunner :: IOWitness (StateT s) -> MVar s -> ResourceRunner '[ StateT s]
mvarResourceRunner iow var = mkResourceRunner iow $ mVarRun var

discardingStateResourceRunner :: IOWitness (StateT s) -> s -> ResourceRunner '[ StateT s]
discardingStateResourceRunner iow s = mkResourceRunner iow $ stateDiscardingUntrans s

exclusiveResourceRunner :: ResourceRunner tt -> LifeCycleIO (ResourceRunner '[ StackT tt])
exclusiveResourceRunner rr = do
    Dict <- return $ resourceRunnerUnliftAllDict rr
    iow <- lift $ newIOWitness
    lifeCycleWith $ \call ->
        runResourceRunner rr $ unStackT $ liftWithUnliftAll $ \unlift -> call $ mkResourceRunner iow unlift

discardingResourceRunner :: ResourceRunner tt -> ResourceRunner tt
discardingResourceRunner (MkResourceRunner run) = MkResourceRunner $ mapListType discardingSingleRunner run
