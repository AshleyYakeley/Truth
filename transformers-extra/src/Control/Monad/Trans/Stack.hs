module Control.Monad.Trans.Stack
    ( TransKind
    , witTransStackDict
    , IsStack
    , transStackDict
    , StackT(..)
    , MonadTransStackSemiTunnel
    , stackLift
    , stackRemonad
    , MonadTransStackTunnel
    , transStackExcept
    , stackUnderliftIO
    , combineIOFunctions
    , stackLiftMBackFunction
    , MonadTransStackUnlift
    , stackLiftWithUnlift
    , MonadTransStackUnliftAll
    , stackCommute
    ) where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Function as I
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Unlift
import Import

type TransKind = (Type -> Type) -> (Type -> Type)

witTransStackDict ::
       forall cm tt m. (cm m)
    => ListType (Compose Dict (MonadTransConstraint cm)) tt
    -> Dict (cm (ApplyStack tt m))
witTransStackDict NilListType = Dict
witTransStackDict (ConsListType (Compose Dict) lt) =
    case witTransStackDict @cm @_ @m lt of
        d -> transConstraintDict @cm d

type IsStack (ct :: TransKind -> Constraint) = Is (ListType (Compose Dict ct))

transStackDict ::
       forall cm tt m. (IsStack (MonadTransConstraint cm) tt, cm m)
    => Dict (cm (ApplyStack tt m))
transStackDict =
    witTransStackDict @cm @tt @m $ representative @_ @(ListType (Compose Dict (MonadTransConstraint cm))) @tt

newtype StackT (tt :: [TransKind]) m a = MkStackT
    { unStackT :: ApplyStack tt m a
    }

instance (IsStack (MonadTransConstraint Monad) tt, Monad m) => Functor (StackT tt m) where
    fmap =
        case transStackDict @Monad @tt @m of
            Dict -> \ab (MkStackT ma) -> MkStackT $ fmap ab ma

instance (IsStack (MonadTransConstraint Monad) tt, Monad m) => Applicative (StackT tt m) where
    pure =
        case transStackDict @Monad @tt @m of
            Dict -> \a -> MkStackT $ pure a
    (<*>) =
        case transStackDict @Monad @tt @m of
            Dict -> \(MkStackT mab) (MkStackT ma) -> MkStackT $ mab <*> ma

instance (IsStack (MonadTransConstraint Monad) tt, Monad m) => Monad (StackT tt m) where
    return = pure
    (>>=) =
        case transStackDict @Monad @tt @m of
            Dict -> \(MkStackT ma) amb -> MkStackT $ ma >>= \a -> unStackT $ amb a

instance (IsStack (MonadTransConstraint Monad) tt, MonadFail m) => MonadFail (StackT tt m) where
    fail s = lift $ fail s

instance (IsStack (MonadTransConstraint Monad) tt, IsStack (MonadTransConstraint MonadFix) tt, MonadFix m) =>
             MonadFix (StackT tt m) where
    mfix :: forall a. (a -> StackT tt m a) -> StackT tt m a
    mfix =
        case transStackDict @MonadFix @tt @m of
            Dict -> \f -> MkStackT $ mfix $ \a -> unStackT $ f a

instance (IsStack (MonadTransConstraint Monad) tt, IsStack (MonadTransConstraint MonadIO) tt, MonadIO m) =>
             MonadIO (StackT tt m) where
    liftIO =
        case transStackDict @MonadIO @tt @m of
            Dict -> \ioa -> MkStackT $ liftIO ioa

instance (IsStack (MonadTransConstraint Monad) tt, IsStack (MonadTransConstraint MonadPlus) tt, MonadPlus m) =>
             Alternative (StackT tt m) where
    empty =
        case transStackDict @MonadPlus @tt @m of
            Dict -> MkStackT empty
    (<|>) =
        case transStackDict @MonadPlus @tt @m of
            Dict -> \(MkStackT a) (MkStackT b) -> MkStackT $ a <|> b

instance (IsStack (MonadTransConstraint Monad) tt, IsStack (MonadTransConstraint MonadPlus) tt, MonadPlus m) =>
             MonadPlus (StackT tt m)

instance (IsStack (MonadTransConstraint Monad) tt) => MonadTrans (StackT tt) where
    lift ::
           forall m a. Monad m
        => m a
        -> StackT tt m a
    lift = let
        build ::
               forall tt'.
               ListType (Compose Dict (MonadTransConstraint Monad)) tt'
            -> (WMFunction m (ApplyStack tt' m), Dict (Monad (ApplyStack tt' m)))
        build NilListType = (id, Dict)
        build (ConsListType (Compose Dict) w) =
            case build w of
                (wmf, dcm@Dict) -> (wLift . wmf, transConstraintDict @Monad dcm)
        in case build $ representative @_ @(ListType (Compose Dict (MonadTransConstraint Monad))) @tt of
               (wmf, _) -> \ma -> MkStackT $ runWMFunction wmf ma

instance (IsStack (MonadTransConstraint Monad) tt) => MonadTransConstraint Monad (StackT tt) where
    hasTransConstraint = Dict

stackLift ::
       forall tt m. (IsStack (MonadTransConstraint Monad) tt, Monad m)
    => MFunction m (ApplyStack tt m)
stackLift ma = unStackT @tt @m $ lift ma

instance (IsStack (MonadTransConstraint Monad) tt, IsStack (MonadTransConstraint MonadFail) tt) =>
             MonadTransConstraint MonadFail (StackT tt) where
    hasTransConstraint = Dict

instance (IsStack (MonadTransConstraint Monad) tt, IsStack (MonadTransConstraint MonadFix) tt) =>
             MonadTransConstraint MonadFix (StackT tt) where
    hasTransConstraint = Dict

instance (IsStack (MonadTransConstraint Monad) tt, IsStack (MonadTransConstraint MonadIO) tt) =>
             MonadTransConstraint MonadIO (StackT tt) where
    hasTransConstraint = Dict

instance (IsStack (MonadTransConstraint Monad) tt, IsStack (MonadTransConstraint MonadPlus) tt) =>
             MonadTransConstraint MonadPlus (StackT tt) where
    hasTransConstraint = Dict

newtype SemiTunnel t = MkSemiTunnel
    { unSemiTunnel :: forall m1 m2 r. (Monad m1, Monad m2) => (forall a. (t m1 r -> m1 a) -> m2 a) -> t m2 r
    }

type MonadTransStackSemiTunnel tt = (IsStack (MonadTransConstraint Monad) tt, IsStack MonadTransSemiTunnel tt)

instance MonadTransStackSemiTunnel tt => MonadTransSemiTunnel (StackT tt) where
    semitunnel ::
           forall m1 m2 r. (Monad m1, Monad m2)
        => (forall a. (StackT tt m1 r -> m1 a) -> m2 a)
        -> StackT tt m2 r
    semitunnel = let
        build :: forall tt'. ListType (Compose Dict MonadTransSemiTunnel) tt' -> SemiTunnel (StackT tt')
        build NilListType = MkSemiTunnel $ \call -> MkStackT $ call $ unStackT
        build (ConsListType (Compose Dict) w) =
            case build w of
                MkSemiTunnel semitunnel' -> let
                    semitunnel'' ::
                           forall m1' m2' r'. (Monad m1', Monad m2')
                        => (forall a. (StackT tt' m1' r' -> m1' a) -> m2' a)
                        -> StackT tt' m2' r'
                    semitunnel'' call =
                        MkStackT $
                        case ( witTransStackDict @Monad @_ @m1' $ mapListType (\(Compose Dict) -> Compose Dict) w
                             , witTransStackDict @Monad @_ @m2' $ mapListType (\(Compose Dict) -> Compose Dict) w) of
                            (Dict, Dict) ->
                                semitunnel $ \unlift1 ->
                                    unStackT $
                                    semitunnel' $ \unlift2 -> call $ \(MkStackT stt) -> unlift2 $ MkStackT $ unlift1 stt
                    in MkSemiTunnel semitunnel''
        in unSemiTunnel $ build $ representative @_ @(ListType (Compose Dict MonadTransSemiTunnel)) @tt

newtype Tunnel t = MkTunnel
    { unTunnel :: forall m2 r. (forall a. (forall m1. t m1 r -> m1 a) -> m2 a) -> t m2 r
    }

newtype TransExcept e tt = MkTransExcept
    { unTransExcept :: forall m a. Monad m => StackT tt (ExceptT e m) a -> StackT tt m (Either e a)
    }

type MonadTransStackTunnel tt = (MonadTransStackSemiTunnel tt, IsStack MonadTransTunnel tt)

instance MonadTransStackTunnel tt => MonadTransTunnel (StackT tt) where
    tunnel :: forall m2 r. (forall a. (forall m1. StackT tt m1 r -> m1 a) -> m2 a) -> StackT tt m2 r
    tunnel = let
        build :: forall tt'. ListType (Compose Dict MonadTransTunnel) tt' -> Tunnel (StackT tt')
        build NilListType = MkTunnel $ \call -> MkStackT $ call $ unStackT
        build (ConsListType (Compose Dict) w) =
            case build w of
                MkTunnel tunnel' -> let
                    tunnel'' ::
                           forall m2' r'.
                           (forall a. (forall m1. StackT tt' m1 r' -> m1 a) -> m2' a)
                        -> StackT tt' m2' r'
                    tunnel'' call =
                        MkStackT $
                        tunnel $ \unlift1 ->
                            unStackT $ tunnel' $ \unlift2 -> call $ \(MkStackT stt) -> unlift2 $ MkStackT $ unlift1 stt
                    in MkTunnel tunnel''
        in unTunnel $ build $ representative @_ @(ListType (Compose Dict MonadTransTunnel)) @tt
    transExcept ::
           forall m e a. Monad m
        => StackT tt (ExceptT e m) a
        -> StackT tt m (Either e a)
    transExcept = let
        build :: forall tt'. ListType (Compose Dict MonadTransTunnel) tt' -> TransExcept e tt'
        build NilListType = MkTransExcept $ \(MkStackT (ExceptT ema)) -> MkStackT ema
        build (ConsListType (Compose Dict) (w :: ListType _ tt0)) =
            case build w of
                MkTransExcept transExcept' -> let
                    transExcept'' ::
                           forall m' a'. Monad m'
                        => StackT tt' (ExceptT e m') a'
                        -> StackT tt' m' (Either e a')
                    transExcept'' (MkStackT aea) =
                        case ( witTransStackDict @Monad @tt0 @m' $ mapListType (\(Compose Dict) -> Compose Dict) w
                             , witTransStackDict @Monad @tt0 @(ExceptT e m') $
                               mapListType (\(Compose Dict) -> Compose Dict) w) of
                            (Dict, Dict) ->
                                MkStackT $
                                transExcept $
                                remonad
                                    (\ttea -> ExceptT $ unStackT $ transExcept' $ MkStackT @tt0 @(ExceptT e m') ttea)
                                    aea
                    in MkTransExcept transExcept''
        in unTransExcept $ build $ representative @_ @(ListType (Compose Dict MonadTransTunnel)) @tt

stackRemonad ::
       forall tt ma mb. (MonadTransStackSemiTunnel tt, Monad ma, Monad mb)
    => MFunction ma mb
    -> MFunction (ApplyStack tt ma) (ApplyStack tt mb)
stackRemonad mf asta = unStackT $ remonad mf $ MkStackT @tt asta

transStackExcept ::
       forall tt m e a. (MonadTransStackTunnel tt, Monad m)
    => ApplyStack tt (ExceptT e m) a
    -> ApplyStack tt m (Either e a)
transStackExcept ata = unStackT $ transExcept $ MkStackT @tt @(ExceptT e m) ata

stackUnderliftIO ::
       forall tt m. (MonadTransStackSemiTunnel tt, MonadIO m)
    => MFunction (ApplyStack tt IO) (ApplyStack tt m)
stackUnderliftIO = stackRemonad @tt @IO @m liftIO

combineIOFunctions ::
       forall tt m. (MonadTransStackSemiTunnel tt, Monad m)
    => IOFunction (ApplyStack tt IO)
    -> IOFunction m
    -> IOFunction (ApplyStack tt m)
combineIOFunctions fa fb = runWMFunction $ MkWMFunction fa . MkWMFunction (stackRemonad @tt fb)

stackLiftWMBackFunction ::
       forall tt ma mb. (MonadTransStackUnliftAll tt, Monad ma, Monad mb)
    => WMBackFunction ma mb
    -> WMBackFunction (ApplyStack tt ma) (ApplyStack tt mb)
stackLiftWMBackFunction mab = coerce $ liftWMBackFunction @(StackT tt) mab

stackLiftMBackFunction ::
       forall tt ma mb. (MonadTransStackUnliftAll tt, Monad ma, Monad mb)
    => MBackFunction ma mb
    -> MBackFunction (ApplyStack tt ma) (ApplyStack tt mb)
stackLiftMBackFunction f = runWMBackFunction $ stackLiftWMBackFunction @tt $ MkWMBackFunction f

type MonadTransStackUnlift tt
     = ( IsStack (MonadTransConstraint MonadFail) tt
       , IsStack (MonadTransConstraint MonadIO) tt
       , IsStack (MonadTransConstraint MonadFix) tt
       , IsStack (MonadTransConstraint MonadUnliftIO) tt
       , MonadTransStackSemiTunnel tt
       , IsStack MonadTransUnlift tt)

newtype LiftWithUnlift t = MkLiftWithUnlift
    { unLiftWithUnlift :: forall m. MonadUnliftIO m => MBackFunction m (t m)
    }

newtype GetDiscardingUnlift t = MkGetDiscardingUnlift
    { unGetDiscardingUnlift :: forall m. MonadUnliftIO m => t m (WMFunction (t m) m)
    }

instance MonadTransStackUnlift tt => MonadTransUnlift (StackT tt) where
    liftWithUnlift ::
           forall m. MonadUnliftIO m
        => MBackFunction m (StackT tt m)
    liftWithUnlift = let
        build :: forall tt'. ListType (Compose Dict MonadTransUnlift) tt' -> LiftWithUnlift (StackT tt')
        build NilListType = MkLiftWithUnlift $ \call -> MkStackT $ call unStackT
        build (ConsListType (Compose Dict) (w :: ListType _ tt0)) =
            case build w of
                MkLiftWithUnlift liftWithUnlift' -> let
                    liftWithUnlift'' ::
                           forall m'. MonadUnliftIO m'
                        => MBackFunction m' (StackT tt' m')
                    liftWithUnlift'' call =
                        MkStackT $
                        case witTransStackDict @MonadUnliftIO @tt0 @m' $ mapListType (\(Compose Dict) -> Compose Dict) w of
                            Dict ->
                                liftWithUnlift $ \unlift1 ->
                                    unStackT $
                                    liftWithUnlift' $ \unlift2 ->
                                        call $ \(MkStackT stt) -> unlift2 $ MkStackT $ unlift1 stt
                    in MkLiftWithUnlift liftWithUnlift''
        in unLiftWithUnlift $ build $ representative @_ @(ListType (Compose Dict MonadTransUnlift)) @tt
    getDiscardingUnlift ::
           forall m. MonadUnliftIO m
        => StackT tt m (WMFunction (StackT tt m) m)
    getDiscardingUnlift = let
        build :: forall tt'. ListType (Compose Dict MonadTransUnlift) tt' -> GetDiscardingUnlift (StackT tt')
        build NilListType = MkGetDiscardingUnlift $ MkStackT $ return $ MkWMFunction unStackT
        build (ConsListType (Compose (Dict :: Dict (MonadTransUnlift t))) (w :: ListType _ tt0)) =
            case build w of
                MkGetDiscardingUnlift getDiscardingUnlift' -> let
                    getDiscardingUnlift'' ::
                           forall m'. MonadUnliftIO m'
                        => StackT tt' m' (WMFunction (StackT tt' m') m')
                    getDiscardingUnlift'' =
                        MkStackT $
                        case witTransStackDict @MonadUnliftIO @tt0 @m' $ mapListType (\(Compose Dict) -> Compose Dict) w of
                            Dict ->
                                withTransConstraintTM @Monad $ do
                                    unlift1 <- getDiscardingUnlift
                                    unlift2 <- lift $ unStackT getDiscardingUnlift'
                                    return $ unlift2 . coerce unlift1
                    in MkGetDiscardingUnlift getDiscardingUnlift''
        in unGetDiscardingUnlift $ build $ representative @_ @(ListType (Compose Dict MonadTransUnlift)) @tt

stackLiftWithUnlift ::
       forall tt m. (MonadTransStackUnlift tt, MonadUnliftIO m)
    => MBackFunction m (ApplyStack tt m)
stackLiftWithUnlift = runWMBackFunction $ coerce $ MkWMBackFunction $ liftWithUnlift @(StackT tt) @m

type MonadTransStackUnliftAll tt
     = ( IsStack (MonadTransConstraint MonadPlus) tt
       , IsStack MonadTransTunnel tt
       , MonadTransStackUnlift tt
       , IsStack MonadTransUnliftAll tt)

newtype LiftWithUnliftAll t = MkLiftWithUnliftAll
    { unLiftWithUnliftAll :: forall m r. MonadUnliftIO m => (UnliftAll t -> m r) -> t m r
    }

newtype GetDiscardingUnliftAll t = MkGetDiscardingUnliftAll
    { unGetDiscardingUnliftAll :: forall m. Monad m => t m (WUnliftAll t)
    }

stackJoinUnliftAll ::
       ListType (Compose Dict MonadTransUnliftAll) tt
    -> UnliftAll t
    -> UnliftAll (StackT tt)
    -> UnliftAll (StackT (t ': tt))
stackJoinUnliftAll w tmm stmm (stma :: StackT (t ': tt) m a) =
    case witTransStackDict @MonadUnliftIO @tt @m $ mapListType (\(Compose Dict) -> Compose Dict) w of
        Dict -> stmm $ MkStackT $ tmm $ unStackT stma

instance MonadTransStackUnliftAll tt => MonadTransUnliftAll (StackT tt) where
    liftWithUnliftAll ::
           forall m r. MonadUnliftIO m
        => (UnliftAll (StackT tt) -> m r)
        -> StackT tt m r
    liftWithUnliftAll = let
        build :: forall tt'. ListType (Compose Dict MonadTransUnliftAll) tt' -> LiftWithUnliftAll (StackT tt')
        build NilListType = MkLiftWithUnliftAll $ \call -> MkStackT $ call unStackT
        build (ConsListType (Compose Dict) (w :: ListType _ tt0)) =
            case build w of
                MkLiftWithUnliftAll liftWithUnliftAll' -> let
                    liftWithUnliftAll'' ::
                           forall m' r'. MonadUnliftIO m'
                        => (UnliftAll (StackT tt') -> m' r')
                        -> StackT tt' m' r'
                    liftWithUnliftAll'' call =
                        MkStackT $
                        case witTransStackDict @MonadUnliftIO @tt0 @m' $ mapListType (\(Compose Dict) -> Compose Dict) w of
                            Dict ->
                                liftWithUnliftAll $ \unlift1 ->
                                    unStackT $
                                    liftWithUnliftAll' $ \unlift2 -> call $ stackJoinUnliftAll w unlift1 unlift2
                    in MkLiftWithUnliftAll liftWithUnliftAll''
        in unLiftWithUnliftAll $ build $ representative @_ @(ListType (Compose Dict MonadTransUnliftAll)) @tt
    getDiscardingUnliftAll ::
           forall m. Monad m
        => StackT tt m (WUnliftAll (StackT tt))
    getDiscardingUnliftAll = let
        build :: forall tt'. ListType (Compose Dict MonadTransUnliftAll) tt' -> GetDiscardingUnliftAll (StackT tt')
        build NilListType = MkGetDiscardingUnliftAll $ MkStackT $ return $ MkWUnliftAll unStackT
        build (ConsListType (Compose (Dict :: Dict (MonadTransUnliftAll t))) (w :: ListType _ tt0)) =
            case build w of
                MkGetDiscardingUnliftAll getDiscardingUnliftAll' -> let
                    getDiscardingUnliftAll'' ::
                           forall m'. Monad m'
                        => StackT tt' m' (WUnliftAll (StackT tt'))
                    getDiscardingUnliftAll'' =
                        MkStackT $
                        case witTransStackDict @Monad @tt0 @m' $ mapListType (\(Compose Dict) -> Compose Dict) w of
                            Dict ->
                                withTransConstraintTM @Monad $ do
                                    MkWUnliftAll unlift1 <- getDiscardingUnliftAll
                                    MkWUnliftAll unlift2 <- lift $ unStackT $ getDiscardingUnliftAll' @m'
                                    return $ MkWUnliftAll $ stackJoinUnliftAll w unlift1 unlift2
                    in MkGetDiscardingUnliftAll getDiscardingUnliftAll''
        in unGetDiscardingUnliftAll $ build $ representative @_ @(ListType (Compose Dict MonadTransUnliftAll)) @tt

stackCommute ::
       forall tta ttb m r. (MonadTransStackUnliftAll tta, MonadTransStackUnliftAll ttb, MonadUnliftIO m)
    => ApplyStack tta (ApplyStack ttb m) r
    -> ApplyStack ttb (ApplyStack tta m) r
stackCommute aar =
    case (transStackDict @MonadUnliftIO @tta @m, transStackDict @MonadUnliftIO @ttb @m) of
        (Dict, Dict) -> let
            ssr :: StackT tta (StackT ttb m) r
            ssr = MkStackT $ stackRemonad @tta @(ApplyStack ttb m) @(StackT ttb m) MkStackT aar
            in stackRemonad @ttb @(StackT tta m) @(ApplyStack tta m) unStackT $ unStackT $ commuteT ssr
