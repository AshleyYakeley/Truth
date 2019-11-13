module Truth.Core.Resource.Runner where

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

unliftStackTransStackRunner :: MonadTransStackUnliftAll tt => UnliftAll (StackT tt) -> TransStackRunner tt
unliftStackTransStackRunner ua = MkTransStackRunner $ \ama -> ua $ MkStackT ama

unliftTransStackRunner :: MonadTransUnliftAll t => UnliftAll t -> TransStackRunner '[ t]
unliftTransStackRunner = MkTransStackRunner

mVarTransStackRunner :: MVar s -> TransStackRunner '[ StateT s]
mVarTransStackRunner var = unliftTransStackRunner $ mVarRun var

instance ConcatMonoid TransStackRunner where
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

data TransListFunction (tt1 :: [TransKind]) (tt2 :: [TransKind]) = MkTransListFunction
    { tlfFunction :: forall m. Monad m => Proxy m -> MFunction (ApplyStack tt1 m) (ApplyStack tt2 m)
    , tlfBackFunction :: forall m. MonadUnliftIO m => Proxy m -> MBackFunction (ApplyStack tt1 m) (ApplyStack tt2 m)
    }

instance Category TransListFunction where
    id = MkTransListFunction (\_ -> runWMFunction id) (\_ -> runWMBackFunction id)
    MkTransListFunction fbc bfbc . MkTransListFunction fab bfab =
        MkTransListFunction
            (\p -> runWMFunction $ MkWMFunction (fbc p) . MkWMFunction (fab p))
            (\p -> runWMBackFunction $ MkWMBackFunction (bfbc p) . MkWMBackFunction (bfab p))

concatFstMFunction ::
       forall tt1 tt2 m. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2, Monad m)
    => MFunction (ApplyStack tt1 m) (ApplyStack (Concat tt1 tt2) m)
concatFstMFunction =
    case transStackConcatRefl @tt1 @tt2 @m of
        Refl ->
            case transStackDict @Monad @tt2 @m of
                Dict -> stackRemonad @tt1 $ stackLift @tt2 @m

concatSndMFunction ::
       forall tt1 tt2 m. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2, Monad m)
    => MFunction (ApplyStack tt2 m) (ApplyStack (Concat tt1 tt2) m)
concatSndMFunction =
    case transStackConcatRefl @tt1 @tt2 @m of
        Refl ->
            case transStackDict @Monad @tt2 @m of
                Dict -> stackLift @tt1

fstTransListFunction ::
       forall tt1 tt2. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2)
    => TransListFunction tt1 (Concat tt1 tt2)
fstTransListFunction = let
    tlfFunction ::
           forall m. Monad m
        => Proxy m
        -> MFunction (ApplyStack tt1 m) (ApplyStack (Concat tt1 tt2) m)
    tlfFunction _ = concatFstMFunction @tt1 @tt2 @m
    tlfBackFunction ::
           forall m. MonadUnliftIO m
        => Proxy m
        -> MBackFunction (ApplyStack tt1 m) (ApplyStack (Concat tt1 tt2) m)
    tlfBackFunction _ =
        case transStackConcatRefl @tt1 @tt2 @m of
            Refl ->
                case transStackDict @MonadUnliftIO @tt2 @m of
                    Dict -> stackLiftMBackFunction @tt1 $ stackLiftWithUnlift @tt2 @m
    in MkTransListFunction {..}

sndTransListFunction ::
       forall tt1 tt2. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2)
    => TransListFunction tt2 (Concat tt1 tt2)
sndTransListFunction = let
    tlfFunction ::
           forall m. Monad m
        => Proxy m
        -> MFunction (ApplyStack tt2 m) (ApplyStack (Concat tt1 tt2) m)
    tlfFunction _ = concatSndMFunction @tt1 @tt2 @m
    tlfBackFunction ::
           forall m. MonadUnliftIO m
        => Proxy m
        -> MBackFunction (ApplyStack tt2 m) (ApplyStack (Concat tt1 tt2) m)
    tlfBackFunction _ =
        case transStackConcatRefl @tt1 @tt2 @m of
            Refl ->
                case transStackDict @MonadUnliftIO @tt2 @m of
                    Dict -> stackLiftWithUnlift @tt1
    in MkTransListFunction {..}

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
