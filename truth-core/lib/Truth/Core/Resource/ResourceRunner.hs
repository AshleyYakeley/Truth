module Truth.Core.Resource.ResourceRunner
    ( ResourceRunner
    , nilResourceRunner
    , combineResourceRunners
    , resourceRunnerUnliftAllDict
    , runResourceRunner
    , runMonoResourceRunner
    , staticResourceRunner
    , dynamicResourceRunner
    , stateResourceRunner
    , discardingResourceRunner
    ) where

import Truth.Core.Import
import Truth.Core.Resource.Function
import Truth.Core.Resource.SingleRunner

newtype ResourceRunner tt =
    MkResourceRunner (ListType SingleRunner tt)

nilResourceRunner :: ResourceRunner '[]
nilResourceRunner = MkResourceRunner NilListType

mapResourceRunner ::
       forall (ct :: TransKind -> Constraint) (tt :: [TransKind]). (forall t. MonadTransUnliftAll t => ct t)
    => ListType SingleRunner tt
    -> ListType (Compose Dict ct) tt
mapResourceRunner = mapListType $ singleRunnerComposeDict @ct

combineLSR ::
       ListType SingleRunner tta
    -> ListType SingleRunner ttb
    -> (forall ttab. ListType SingleRunner ttab -> TransListFunction (Concat tta ttb) ttab -> r)
    -> r
combineLSR NilListType rb f = f rb id
combineLSR ra NilListType f =
    case concatEmptyRefl ra of
        Refl -> f ra id
combineLSR au1@(ConsListType u1 uu1) au2@(ConsListType u2 uu2) f =
    case singleRunnerUnliftAllDict u1 of
        Dict ->
            case singleRunnerUnliftAllDict u2 of
                Dict ->
                    case singleRunnerOrder u1 u2 of
                        SREQ ->
                            combineLSR uu1 uu2 $ \uu12 tf ->
                                f (ConsListType u1 uu12) $
                                consTransListFunction
                                    (mapResourceRunner $ concatListType uu1 uu2)
                                    (mapResourceRunner uu12)
                                    tf .
                                contractTransListFunction (mapResourceRunner $ concatListType uu1 uu2) .
                                reorderTransListFunction (mapResourceRunner au1) (mapResourceRunner uu2)
                        SRLT ->
                            combineLSR uu1 au2 $ \uu12 tf ->
                                f (ConsListType u1 uu12) $
                                consTransListFunction
                                    (mapResourceRunner $ concatListType uu1 au2)
                                    (mapResourceRunner uu12)
                                    tf
                        SRGT ->
                            combineLSR au1 uu2 $ \uu12 tf ->
                                f (ConsListType u2 uu12) $
                                consTransListFunction
                                    (mapResourceRunner $ concatListType au1 uu2)
                                    (mapResourceRunner uu12)
                                    tf .
                                reorderTransListFunction (mapResourceRunner au1) (mapResourceRunner uu2)

combineResourceRunners ::
       ResourceRunner tta
    -> ResourceRunner ttb
    -> (forall ttab. ResourceRunner ttab -> TransListFunction (Concat tta ttb) ttab -> r)
    -> r
combineResourceRunners (MkResourceRunner la) (MkResourceRunner lb) call =
    combineLSR la lb $ \lab -> call (MkResourceRunner lab)

resourceRunnerUnliftAllDict :: ResourceRunner tt -> Dict (MonadTransStackUnliftAll tt)
resourceRunnerUnliftAllDict (MkResourceRunner lsr) = let
    build :: forall tt'. ListType SingleRunner tt' -> Dict (MonadTransStackUnliftAll tt')
    build NilListType = Dict
    build (ConsListType (singleRunnerUnliftAllDict -> Dict) (build -> Dict)) = Dict
    in build lsr

runResourceRunner ::
       forall tt r.
       ResourceRunner tt
    -> (MonadTransStackUnliftAll tt => (forall m. MonadUnliftIO m => MFunction (ApplyStack tt m) m) -> r)
    -> r
runResourceRunner (MkResourceRunner lsr) = let
    build ::
           forall tt' r'.
           ListType SingleRunner tt'
        -> (MonadTransStackUnliftAll tt' =>
                    (forall m.
                         MonadUnliftIO m => (WMFunction (ApplyStack tt' m) m, Dict (MonadUnliftIO (ApplyStack tt' m)))) -> r')
        -> r'
    build NilListType call = call (id, Dict)
    build (ConsListType sr w) call =
        build w $ \f1d ->
            runSingleRunner sr $ \fr ->
                call $
                case f1d of
                    (f1, dm@Dict) -> (f1 . MkWMFunction fr, transConstraintDict @MonadUnliftIO dm)
    in \call -> build lsr $ \mfd -> call $ runWMFunction $ fst mfd

runMonoResourceRunner ::
       forall m tt r. MonadUnliftIO m
    => ResourceRunner tt
    -> ((MonadTransStackUnliftAll tt, MonadUnliftIO (ApplyStack tt m)) => (MFunction (ApplyStack tt m) m) -> r)
    -> r
runMonoResourceRunner rr call =
    runResourceRunner rr $
    case transStackDict @MonadUnliftIO @tt @m of
        Dict -> call

singleResourceRunner :: SingleRunner t -> ResourceRunner '[ t]
singleResourceRunner sr = MkResourceRunner $ ConsListType sr NilListType

staticResourceRunner ::
       forall t. MonadTransAskUnlift t
    => IOWitness t
    -> UnliftAll MonadUnliftIO t
    -> ResourceRunner '[ t]
staticResourceRunner iow run = singleResourceRunner $ StaticSingleRunner iow run

dynamicResourceRunner ::
       forall t. MonadTransUnliftAll t
    => IO (WUnliftAll MonadUnliftIO t)
    -> ResourceRunner '[ t]
dynamicResourceRunner iorun = singleResourceRunner $ DynamicSingleRunner iorun

stateResourceRunner :: s -> ResourceRunner '[ StateT s]
stateResourceRunner s =
    dynamicResourceRunner $ do
        var <- newMVar s
        return $ wMVarRun var

discardingResourceRunner :: forall tt. ResourceRunner tt -> ResourceRunner tt
discardingResourceRunner (MkResourceRunner run) = MkResourceRunner $ mapListType discardingSingleRunner run
