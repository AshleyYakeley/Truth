module Changes.Core.Resource.ResourceRunner
    ( ResourceRunner
    , nilResourceRunner
    , combineIndependentResourceRunners
    , combineResourceRunners
    , resourceRunnerUnliftAllDict
    , resourceRunnerStackUnliftDict
    , mkResourceRunner
    , newResourceRunner
    , stateResourceRunner
    , mvarResourceRunner
    , discardingStateResourceRunner
    , exclusiveResourceRunner
    , discardingResourceRunner
    , ResourceContext
    , emptyResourceContext
    , runResourceRunner
    , runResourceRunnerContext
    ) where

import Changes.Core.Import
import Changes.Core.Resource.Function
import Changes.Core.Resource.SingleRunner
import Debug.ThreadTrace

newtype ResourceRunner (tt :: [TransKind]) =
    MkResourceRunner (ListType SingleRunner tt)

nilResourceRunner :: ResourceRunner '[]
nilResourceRunner = MkResourceRunner NilListType

mapResourceRunner ::
       forall (ct :: TransKind -> Constraint) (tt :: [TransKind]). (forall t. MonadTransUnlift t => ct t)
    => ListType SingleRunner tt
    -> ListType (Compose Dict ct) tt
mapResourceRunner =
    mapListType $ \sr ->
        case singleRunnerUnliftAllDict sr of
            Dict -> Compose Dict

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

lsrUnliftAllDict :: ListType SingleRunner tt -> Dict (MonadTransStackUnlift tt)
lsrUnliftAllDict NilListType = Dict
lsrUnliftAllDict (ConsListType (singleRunnerUnliftAllDict -> Dict) (lsrUnliftAllDict -> Dict)) = Dict

resourceRunnerUnliftAllDict :: ResourceRunner tt -> Dict (MonadTransStackUnlift tt)
resourceRunnerUnliftAllDict (MkResourceRunner lsr) = lsrUnliftAllDict lsr

resourceRunnerStackUnliftDict ::
       forall m tt. MonadUnliftIO m
    => ResourceRunner tt
    -> Dict (MonadUnliftIO (ApplyStack tt m))
resourceRunnerStackUnliftDict rr =
    case resourceRunnerUnliftAllDict rr of
        Dict -> transStackDict @MonadUnliftIO @tt @m

singleResourceRunner :: SingleRunner t -> ResourceRunner '[ t]
singleResourceRunner sr = MkResourceRunner $ ConsListType sr NilListType

mkResourceRunner ::
       forall t. MonadTransUnlift t
    => IOWitness t
    -> Unlift MonadUnliftIO t
    -> ResourceRunner '[ t]
mkResourceRunner iow run = singleResourceRunner $ mkSingleRunner iow run

newResourceRunner ::
       forall t. MonadTransUnlift t
    => Unlift MonadUnliftIO t
    -> IO (ResourceRunner '[ t])
newResourceRunner run = do
    iow <- newIOWitness
    return $ mkResourceRunner iow run

stateResourceRunner :: s -> IO (ResourceRunner '[ StateT s])
stateResourceRunner s = do
    var <- newMVar s
    newResourceRunner $ traceBarrier "stateResourceRunner.mVar" $ mVarRun var

mvarResourceRunner :: IOWitness (StateT s) -> MVar s -> ResourceRunner '[ StateT s]
mvarResourceRunner iow var = mkResourceRunner iow $ traceBarrier "mvarResourceRunner.mVar" $ mVarRun var

discardingStateResourceRunner :: IOWitness (StateT s) -> s -> ResourceRunner '[ StateT s]
discardingStateResourceRunner iow s = mkResourceRunner iow $ stateDiscardingUntrans s

discardingResourceRunner :: ResourceRunner tt -> ResourceRunner tt
discardingResourceRunner (MkResourceRunner run) = MkResourceRunner $ mapListType discardingSingleRunner run

newtype ResourceContext =
    MkResourceContext [AnyW SingleRunner]

instance Show ResourceContext where
    show (MkResourceContext rc) = "{" <> show (length rc) <> "}"

emptyResourceContext :: ResourceContext
emptyResourceContext = MkResourceContext []

runLSR ::
       forall tt m r. MonadUnliftIO m
    => [AnyW SingleRunner]
    -> ListType SingleRunner tt
    -> ((MonadTransStackUnlift tt, MonadUnliftIO (ApplyStack tt m)) => ApplyStack tt m r)
    -> m r
runLSR _ NilListType call = call
runLSR rc (ConsListType (sr :: _ t) (lsr :: _ tt0)) call =
    case singleRunnerUnliftAllDict sr of
        Dict ->
            case hasTransConstraint @MonadUnliftIO @t @m of
                Dict ->
                    runLSR rc lsr $
                    runSingleRunner rc sr $
                    case hasTransConstraint @MonadUnliftIO @t @(ApplyStack tt0 m) of
                        Dict -> call

runResourceRunner ::
       forall tt m r. MonadUnliftIO m
    => ResourceContext
    -> ResourceRunner tt
    -> ((MonadTransStackUnlift tt, MonadUnliftIO (ApplyStack tt m)) => ApplyStack tt m r)
    -> m r
runResourceRunner (MkResourceContext rc) (MkResourceRunner rr) call = traceBracket "runResourceRunner: outside" $ runLSR rc rr $ traceBracket "runResourceRunner: inside" $ call

runLSRContext ::
       forall tt m r. MonadUnliftIO m
    => [AnyW SingleRunner]
    -> ListType SingleRunner tt
    -> ((MonadTransStackUnlift tt, MonadUnliftIO (ApplyStack tt m)) => [AnyW SingleRunner] -> WStackUnliftAll tt -> m r)
    -> m r
runLSRContext rc NilListType call = call rc $ MkWStackUnliftAll id
runLSRContext rc (ConsListType (sr :: _ t) (lsr :: _ tt0)) call =
    runLSRContext rc lsr $ \rc' unliftr ->
        runSingleRunnerContext rc' sr $ \rc'' unlift1 ->
            case hasTransConstraint @MonadUnliftIO @t @(ApplyStack tt0 m) of
                Dict ->
                    case transStackDict @MonadUnliftIO @tt0 @m of
                        Dict -> call rc'' $ consWStackUnliftAll (MkWUnlift unlift1) unliftr

runResourceRunnerContext ::
       forall tt m r. MonadUnliftIO m
    => ResourceContext
    -> ResourceRunner tt
    -> ((MonadTransStackUnlift tt, MonadUnliftIO (ApplyStack tt m)) => ResourceContext -> StackUnliftAll tt -> m r)
    -> m r
runResourceRunnerContext (MkResourceContext rc) (MkResourceRunner rr) call = traceBracket "runResourceRunnerContext: outside" $
    runLSRContext rc rr $ \rc' (MkWStackUnliftAll unlift) -> traceBracket "runResourceRunnerContext: inside" $ call (MkResourceContext rc') $ unlift

exclusiveResourceRunner :: ResourceContext -> ResourceRunner tt -> LifeCycle (ResourceRunner '[ StackT tt])
exclusiveResourceRunner rc rr = do
    Dict <- return $ resourceRunnerUnliftAllDict rr
    iow <- liftIO $ newIOWitness
    lifeCycleWith $ \call ->
        runResourceRunnerContext rc rr $ \_ unlift -> call $ mkResourceRunner iow $ \(MkStackT tma) -> unlift tma
