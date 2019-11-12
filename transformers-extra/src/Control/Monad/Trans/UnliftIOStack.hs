module Control.Monad.Trans.UnliftIOStack where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Function
import Control.Monad.Trans.Stack
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Unlift
import Import

type MonadTransStackUnliftAllWitness = ListType (Compose Dict MonadTransUnliftAll)

monadTransStackUnliftAllWitness ::
       forall tt. MonadTransStackUnliftAll tt
    => MonadTransStackUnliftAllWitness tt
monadTransStackUnliftAllWitness = representative @_ @MonadTransStackUnliftAllWitness @tt

transStackUnliftMonad ::
       forall tt m. (MonadTransStackUnliftAll tt, Monad m)
    => Dict (Monad (ApplyStack tt m))
transStackUnliftMonad = transStackDict @Monad @tt @m

transStackUnliftMonadIO ::
       forall tt m. (MonadTransStackUnliftAll tt, MonadIO m)
    => Dict (MonadIO (ApplyStack tt m))
transStackUnliftMonadIO = transStackDict @MonadIO @tt @m

transStackUnliftMonadUnliftIO ::
       forall tt m. (MonadTransStackUnliftAll tt, MonadUnliftIO m)
    => Dict (MonadUnliftIO (ApplyStack tt m))
transStackUnliftMonadUnliftIO = transStackDict @MonadUnliftIO @tt @m

transStackConcatRefl ::
       forall (tt1 :: [TransKind]) (tt2 :: [TransKind]) m. MonadTransStackUnliftAll tt1
    => (ApplyStack (Concat tt1 tt2) m) :~: (ApplyStack tt1 (ApplyStack tt2 m))
transStackConcatRefl = applyConcatRefl @_ @tt1 @tt2 @m @(Compose Dict MonadTransUnliftAll)

concatMonadTransStackUnliftAllDict ::
       forall tt1 tt2. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2)
    => Dict (MonadTransStackUnliftAll (Concat tt1 tt2))
concatMonadTransStackUnliftAllDict =
    case concatIsDict @((Compose Dict MonadTransUnliftAll)) @tt1 @tt2 of
        Dict ->
            case concatIsDict @((Compose Dict (MonadTransConstraint MonadPlus))) @tt1 @tt2 of
                Dict ->
                    case concatIsDict @((Compose Dict (MonadTransConstraint MonadFail))) @tt1 @tt2 of
                        Dict ->
                            case concatIsDict @((Compose Dict (MonadTransConstraint MonadIO))) @tt1 @tt2 of
                                Dict ->
                                    case concatIsDict @((Compose Dict (MonadTransConstraint MonadFix))) @tt1 @tt2 of
                                        Dict ->
                                            case concatIsDict
                                                     @((Compose Dict (MonadTransConstraint MonadUnliftIO)))
                                                     @tt1
                                                     @tt2 of
                                                Dict ->
                                                    case concatIsDict
                                                             @((Compose Dict (MonadTransConstraint Monad)))
                                                             @tt1
                                                             @tt2 of
                                                        Dict ->
                                                            case concatIsDict
                                                                     @((Compose Dict MonadTransTunnel))
                                                                     @tt1
                                                                     @tt2 of
                                                                Dict ->
                                                                    case concatIsDict
                                                                             @((Compose Dict MonadTransSemiTunnel))
                                                                             @tt1
                                                                             @tt2 of
                                                                        Dict ->
                                                                            case concatIsDict
                                                                                     @((Compose Dict MonadTransUnlift))
                                                                                     @tt1
                                                                                     @tt2 of
                                                                                Dict -> Dict

data TransStackRunner (tt :: [TransKind]) where
    MkTransStackRunner
        :: MonadTransStackUnliftAll tt
        => (forall m. MonadUnliftIO m => MFunction (ApplyStack tt m) m)
        -> TransStackRunner tt

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
                    case transStackUnliftMonadUnliftIO @tt2 @m of
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
            case transStackUnliftMonad @tt2 @m of
                Dict -> stackRemonad @tt1 $ stackLift @tt2 @m

concatSndMFunction ::
       forall tt1 tt2 m. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2, Monad m)
    => MFunction (ApplyStack tt2 m) (ApplyStack (Concat tt1 tt2) m)
concatSndMFunction =
    case transStackConcatRefl @tt1 @tt2 @m of
        Refl ->
            case transStackUnliftMonad @tt2 @m of
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
                case transStackUnliftMonadUnliftIO @tt2 @m of
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
                case transStackUnliftMonadUnliftIO @tt2 @m of
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
