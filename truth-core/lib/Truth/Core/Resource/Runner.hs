module Truth.Core.Resource.Runner where

import Truth.Core.Import
import Truth.Core.Resource.Function
import Truth.Core.Resource.SingleRunner

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

unliftStackTransStackRunner :: MonadTransStackUnliftAll tt => UnliftAll MonadUnliftIO (StackT tt) -> TransStackRunner tt
unliftStackTransStackRunner ua = MkTransStackRunner $ \ama -> ua $ MkStackT ama

unliftTransStackRunner :: MonadTransUnliftAll t => UnliftAll MonadUnliftIO t -> TransStackRunner '[ t]
unliftTransStackRunner = MkTransStackRunner

mVarTransStackRunner :: MVar s -> TransStackRunner '[ StateT s]
mVarTransStackRunner var = unliftTransStackRunner $ mVarRun var

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

combineUnliftFstMFunction ::
       forall tt (m :: Type -> Type). (MonadTransStackUnliftAll tt, MonadIO m)
    => MFunction (ApplyStack tt IO) (ApplyStack tt m)
combineUnliftFstMFunction = stackUnderliftIO @tt @m

combineUnliftIOFunctions ::
       forall tt m. (MonadTransStackUnliftAll tt, Monad m)
    => IOFunction (ApplyStack tt IO)
    -> IOFunction m
    -> IOFunction (ApplyStack tt m)
combineUnliftIOFunctions = combineIOFunctions @tt @m

type ResourceRunner = ListType SingleRunner

mapResourceRunner ::
       forall (ct :: TransKind -> Constraint) (tt :: [TransKind]). (forall t. MonadTransUnliftAll t => ct t)
    => ResourceRunner tt
    -> ListType (Compose Dict ct) tt
mapResourceRunner =
    mapListType $ \sr ->
        case singleRunnerUnliftAllDict sr of
            Dict -> Compose Dict

combineResourceRunners ::
       ResourceRunner tta
    -> ResourceRunner ttb
    -> (forall ttab. ResourceRunner ttab -> TransListFunction (Concat tta ttb) ttab -> r)
    -> r
combineResourceRunners NilListType rb f = f rb id
combineResourceRunners ra NilListType f =
    case concatEmptyRefl ra of
        Refl -> f ra id
combineResourceRunners au1@(ConsListType u1 uu1) au2@(ConsListType u2 uu2) f =
    case singleRunnerUnliftAllDict u1 of
        Dict ->
            case singleRunnerUnliftAllDict u2 of
                Dict ->
                    case singleRunnerOrder u1 u2 of
                        SREQ ->
                            combineResourceRunners uu1 uu2 $ \uu12 tf ->
                                f (ConsListType u1 uu12) $
                                consTransListFunction
                                    (mapResourceRunner $ concatListType uu1 uu2)
                                    (mapResourceRunner uu12)
                                    tf .
                                contractTransListFunction (mapResourceRunner $ concatListType uu1 uu2) .
                                reorderTransListFunction (mapResourceRunner au1) (mapResourceRunner uu2)
                        SRLT ->
                            combineResourceRunners uu1 au2 $ \uu12 tf ->
                                f (ConsListType u1 uu12) $
                                consTransListFunction
                                    (mapResourceRunner $ concatListType uu1 au2)
                                    (mapResourceRunner uu12)
                                    tf
                        SRGT ->
                            combineResourceRunners au1 uu2 $ \uu12 tf ->
                                f (ConsListType u2 uu12) $
                                consTransListFunction
                                    (mapResourceRunner $ concatListType au1 uu2)
                                    (mapResourceRunner uu12)
                                    tf .
                                reorderTransListFunction (mapResourceRunner au1) (mapResourceRunner uu2)
