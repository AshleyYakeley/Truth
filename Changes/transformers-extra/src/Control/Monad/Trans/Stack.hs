module Control.Monad.Trans.Stack
    ( TransKind
    , witTransStackDict
    , IsStack
    , transStackDict
    , StackT(..)
    , stackLift
    , stackRemonad
    , MonadTransStackTunnel
    , transStackExcept
    , stackUnderliftIO
    , combineIOFunctions
    , stackLiftMBackFunction
    , MonadTransStackUnlift
    , concatMonadTransStackUnliftDict
    , stackLiftWithUnlift
    , MonadTransStackUnliftAll
    , concatMonadTransStackUnliftAllDict
    , concatFstMFunction
    , concatSndMFunction
    , stackCommute
    , transStackConcatRefl
    , StackUnliftAll
    , WStackUnliftAll(..)
    , consWStackUnliftAll
    , stackLiftWithUnliftAll
    ) where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Function
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Unlift
import Data.Functor.One
import Import

witTransStackDict ::
       forall cm tt m. (cm m)
    => ListType (Compose Dict (TransConstraint cm)) tt
    -> Dict (cm (ApplyStack tt m))
witTransStackDict NilListType = Dict
witTransStackDict (ConsListType (Compose Dict) lt) =
    case witTransStackDict @cm @_ @m lt of
        d -> transConstraintDict @cm d

type IsStack (ct :: TransKind -> Constraint) = Is (ListType (Compose Dict ct))

transStackDict ::
       forall cm tt m. (IsStack (TransConstraint cm) tt, cm m)
    => Dict (cm (ApplyStack tt m))
transStackDict = witTransStackDict @cm @tt @m $ representative @_ @(ListType (Compose Dict (TransConstraint cm))) @tt

newtype StackT (tt :: [TransKind]) m a = MkStackT
    { unStackT :: ApplyStack tt m a
    }

instance (IsStack (TransConstraint Functor) tt, Functor m) => Functor (StackT tt m) where
    fmap =
        case transStackDict @Functor @tt @m of
            Dict -> \ab (MkStackT ma) -> MkStackT $ fmap ab ma

instance (IsStack (TransConstraint Functor) tt, IsStack (TransConstraint Monad) tt, Monad m) =>
             Applicative (StackT tt m) where
    pure =
        case transStackDict @Monad @tt @m of
            Dict -> \a -> MkStackT $ pure a
    (<*>) =
        case transStackDict @Monad @tt @m of
            Dict -> \(MkStackT mab) (MkStackT ma) -> MkStackT $ mab <*> ma

instance (IsStack (TransConstraint Functor) tt, IsStack (TransConstraint Monad) tt, Monad m) => Monad (StackT tt m) where
    return = pure
    (>>=) =
        case transStackDict @Monad @tt @m of
            Dict -> \(MkStackT ma) amb -> MkStackT $ ma >>= \a -> unStackT $ amb a

instance (IsStack (TransConstraint Functor) tt, IsStack (TransConstraint Monad) tt, IsStack MonadTrans tt, MonadFail m) =>
             MonadFail (StackT tt m) where
    fail s = lift $ fail s

instance ( IsStack (TransConstraint Functor) tt
         , IsStack (TransConstraint Monad) tt
         , IsStack (TransConstraint MonadFix) tt
         , MonadFix m
         ) => MonadFix (StackT tt m) where
    mfix :: forall a. (a -> StackT tt m a) -> StackT tt m a
    mfix =
        case transStackDict @MonadFix @tt @m of
            Dict -> \f -> MkStackT $ mfix $ \a -> unStackT $ f a

instance ( IsStack (TransConstraint Functor) tt
         , IsStack (TransConstraint Monad) tt
         , IsStack (TransConstraint MonadIO) tt
         , MonadIO m
         ) => MonadIO (StackT tt m) where
    liftIO =
        case transStackDict @MonadIO @tt @m of
            Dict -> \ioa -> MkStackT $ liftIO ioa

instance ( IsStack (TransConstraint Functor) tt
         , IsStack (TransConstraint Monad) tt
         , IsStack (TransConstraint MonadPlus) tt
         , MonadPlus m
         ) => Alternative (StackT tt m) where
    empty =
        case transStackDict @MonadPlus @tt @m of
            Dict -> MkStackT empty
    (<|>) =
        case transStackDict @MonadPlus @tt @m of
            Dict -> \(MkStackT a) (MkStackT b) -> MkStackT $ a <|> b

instance ( IsStack (TransConstraint Functor) tt
         , IsStack (TransConstraint Monad) tt
         , IsStack (TransConstraint MonadPlus) tt
         , MonadPlus m
         ) => MonadPlus (StackT tt m)

instance (IsStack (TransConstraint Functor) tt, IsStack (TransConstraint Monad) tt, IsStack MonadTrans tt) =>
             MonadTrans (StackT tt) where
    lift ::
           forall m a. Monad m
        => m a
        -> StackT tt m a
    lift = let
        build ::
               forall tt'.
               ListType (Compose Dict (TransConstraint Monad)) tt'
            -> ListType (Compose Dict MonadTrans) tt'
            -> (WMFunction m (ApplyStack tt' m), Dict (Monad (ApplyStack tt' m)))
        build NilListType NilListType = (id, Dict)
        build (ConsListType (Compose Dict) wa) (ConsListType (Compose Dict) wb) =
            case build wa wb of
                (wmf, dcm@Dict) -> (wLift . wmf, transConstraintDict @Monad dcm)
        in let
               wa = representative @_ @(ListType (Compose Dict (TransConstraint Monad))) @tt
               wb = representative @_ @(ListType (Compose Dict MonadTrans)) @tt
               in case build wa wb of
                      (wmf, _) -> \ma -> MkStackT $ runWMFunction wmf ma

instance (IsStack (TransConstraint Functor) tt) => TransConstraint Functor (StackT tt) where
    hasTransConstraint = Dict

instance (IsStack (TransConstraint Functor) tt, IsStack (TransConstraint Monad) tt) => TransConstraint Monad (StackT tt) where
    hasTransConstraint = Dict

stackLift ::
       forall tt m.
       (IsStack (TransConstraint Functor) tt, IsStack (TransConstraint Monad) tt, IsStack MonadTrans tt, Monad m)
    => MFunction m (ApplyStack tt m)
stackLift ma = unStackT @tt @m $ lift ma

instance ( IsStack (TransConstraint Functor) tt
         , IsStack (TransConstraint Monad) tt
         , IsStack (TransConstraint MonadFail) tt
         , IsStack MonadTrans tt
         ) => TransConstraint MonadFail (StackT tt) where
    hasTransConstraint = Dict

instance ( IsStack (TransConstraint Functor) tt
         , IsStack (TransConstraint Monad) tt
         , IsStack (TransConstraint MonadFix) tt
         ) => TransConstraint MonadFix (StackT tt) where
    hasTransConstraint = Dict

instance ( IsStack (TransConstraint Functor) tt
         , IsStack (TransConstraint Monad) tt
         , IsStack (TransConstraint MonadIO) tt
         ) => TransConstraint MonadIO (StackT tt) where
    hasTransConstraint = Dict

instance ( IsStack (TransConstraint Functor) tt
         , IsStack (TransConstraint Monad) tt
         , IsStack (TransConstraint MonadPlus) tt
         ) => TransConstraint MonadPlus (StackT tt) where
    hasTransConstraint = Dict

type MonadTransStackTunnel tt
     = ( IsStack (TransConstraint Functor) tt
       , IsStack (TransConstraint Monad) tt
       , IsStack MonadTransTunnel tt
       , IsStack MonadTrans tt)

concatMonadTransStackTunnelDict ::
       forall tt1 tt2. (MonadTransStackTunnel tt1, MonadTransStackTunnel tt2)
    => Dict (MonadTransStackTunnel (Concat tt1 tt2))
concatMonadTransStackTunnelDict =
    case concatIsDict @((Compose Dict (TransConstraint Functor))) @tt1 @tt2 of
        Dict ->
            case concatIsDict @((Compose Dict (TransConstraint Monad))) @tt1 @tt2 of
                Dict ->
                    case concatIsDict @((Compose Dict MonadTransTunnel)) @tt1 @tt2 of
                        Dict ->
                            case concatIsDict @((Compose Dict MonadTrans)) @tt1 @tt2 of
                                Dict -> Dict

newtype Tunnel t = MkTunnel
    { unTunnel :: forall m2 r.
                      Functor m2 =>
                              (forall f. FunctorOne f => (forall m1 a. Functor m1 => t m1 a -> m1 (f a)) -> m2 (f r)) -> t m2 r
    }

instance MonadTransStackTunnel tt => MonadTransTunnel (StackT tt) where
    tunnel ::
           forall m2 r. Functor m2
        => (forall f. FunctorOne f => (forall m1 a. Functor m1 => StackT tt m1 a -> m1 (f a)) -> m2 (f r))
        -> StackT tt m2 r
    tunnel = let
        build :: forall tt'. ListType (Compose Dict MonadTransTunnel) tt' -> Tunnel (StackT tt')
        build NilListType = MkTunnel $ \call -> MkStackT $ fmap runIdentity $ call $ fmap Identity . unStackT
        build (ConsListType (Compose Dict) (w :: ListType _ tt0)) =
            case build w of
                MkTunnel tunnel' -> let
                    tunnel'' ::
                           forall m2' r'. Functor m2'
                        => (forall f.
                                FunctorOne f => (forall m1 a. Functor m1 => StackT tt' m1 a -> m1 (f a)) -> m2' (f r'))
                        -> StackT tt' m2' r'
                    tunnel'' call =
                        case (witTransStackDict @Functor @tt0 @m2' $ mapListType (\(Compose Dict) -> Compose Dict) w) of
                            Dict ->
                                MkStackT $
                                tunnel $ \unlift1 ->
                                    unStackT $
                                    tunnel' $ \unlift2 ->
                                        fmap getCompose $
                                        call $ \(MkStackT stt :: _ m1 _) ->
                                            case (witTransStackDict @Functor @tt0 @m1 $
                                                  mapListType (\(Compose Dict) -> Compose Dict) w) of
                                                Dict -> fmap Compose $ unlift2 $ MkStackT $ unlift1 stt
                    in MkTunnel tunnel''
        in unTunnel $ build $ representative @_ @(ListType (Compose Dict MonadTransTunnel)) @tt

stackRemonad ::
       forall tt ma mb. (MonadTransStackTunnel tt, Monad ma, Monad mb)
    => MFunction ma mb
    -> MFunction (ApplyStack tt ma) (ApplyStack tt mb)
stackRemonad mf asta = unStackT $ remonad mf $ MkStackT @tt asta

transStackExcept ::
       forall tt m e a. (MonadTransStackTunnel tt, Monad m)
    => ApplyStack tt (ExceptT e m) a
    -> ApplyStack tt m (Either e a)
transStackExcept ata = unStackT $ transExcept $ MkStackT @tt @(ExceptT e m) ata

stackUnderliftIO ::
       forall tt m. (MonadTransStackTunnel tt, MonadIO m)
    => MFunction (ApplyStack tt IO) (ApplyStack tt m)
stackUnderliftIO = stackRemonad @tt @IO @m liftIO

combineIOFunctions ::
       forall tt m. (MonadTransStackTunnel tt, Monad m)
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
     = ( IsStack (TransConstraint MonadFail) tt
       , IsStack (TransConstraint MonadIO) tt
       , IsStack (TransConstraint MonadFix) tt
       , IsStack (TransConstraint MonadTunnelIO) tt
       , IsStack (TransConstraint MonadUnliftIO) tt
       , MonadTransStackTunnel tt
       , IsStack MonadTransTunnel tt
       , IsStack MonadTransUnlift tt)

concatMonadTransStackUnliftDict ::
       forall tt1 tt2. (MonadTransStackUnlift tt1, MonadTransStackUnlift tt2)
    => Dict (MonadTransStackUnlift (Concat tt1 tt2))
concatMonadTransStackUnliftDict =
    case concatIsDict @(Compose Dict (TransConstraint MonadFail)) @tt1 @tt2 of
        Dict ->
            case concatIsDict @(Compose Dict (TransConstraint MonadIO)) @tt1 @tt2 of
                Dict ->
                    case concatIsDict @(Compose Dict (TransConstraint MonadFix)) @tt1 @tt2 of
                        Dict ->
                            case concatIsDict @(Compose Dict (TransConstraint MonadTunnelIO)) @tt1 @tt2 of
                                Dict ->
                                    case concatIsDict @(Compose Dict (TransConstraint MonadUnliftIO)) @tt1 @tt2 of
                                        Dict ->
                                            case concatMonadTransStackTunnelDict @tt1 @tt2 of
                                                Dict ->
                                                    case concatIsDict @(Compose Dict MonadTransTunnel) @tt1 @tt2 of
                                                        Dict ->
                                                            case concatIsDict @(Compose Dict MonadTransUnlift) @tt1 @tt2 of
                                                                Dict -> Dict

newtype LiftWithUnlift t = MkLiftWithUnlift
    { unLiftWithUnlift :: forall m. MonadTunnelIO m => MBackFunction m (t m)
    }

newtype GetDiscardingUnlift t = MkGetDiscardingUnlift
    { unGetDiscardingUnlift :: forall m. MonadTunnelIO m => t m (WMFunction (t m) m)
    }

instance MonadTransStackUnlift tt => MonadTransUnlift (StackT tt) where
    liftWithUnlift ::
           forall m. MonadTunnelIO m
        => MBackFunction m (StackT tt m)
    liftWithUnlift = let
        build :: forall tt'. ListType (Compose Dict MonadTransUnlift) tt' -> LiftWithUnlift (StackT tt')
        build NilListType = MkLiftWithUnlift $ \call -> MkStackT $ call unStackT
        build (ConsListType (Compose Dict) (w :: ListType _ tt0)) =
            case build w of
                MkLiftWithUnlift liftWithUnlift' -> let
                    liftWithUnlift'' ::
                           forall m'. MonadTunnelIO m'
                        => MBackFunction m' (StackT tt' m')
                    liftWithUnlift'' call =
                        MkStackT $
                        case witTransStackDict @MonadTunnelIO @tt0 @m' $ mapListType (\(Compose Dict) -> Compose Dict) w of
                            Dict ->
                                liftWithUnlift $ \unlift1 ->
                                    unStackT $
                                    liftWithUnlift' $ \unlift2 ->
                                        call $ \(MkStackT stt) -> unlift2 $ MkStackT $ unlift1 stt
                    in MkLiftWithUnlift liftWithUnlift''
        in unLiftWithUnlift $ build $ representative @_ @(ListType (Compose Dict MonadTransUnlift)) @tt
    getDiscardingUnlift ::
           forall m. MonadTunnelIO m
        => StackT tt m (WMFunction (StackT tt m) m)
    getDiscardingUnlift = let
        build :: forall tt'. ListType (Compose Dict MonadTransUnlift) tt' -> GetDiscardingUnlift (StackT tt')
        build NilListType = MkGetDiscardingUnlift $ MkStackT $ return $ MkWMFunction unStackT
        build (ConsListType (Compose (Dict :: Dict (MonadTransUnlift t))) (w :: ListType _ tt0)) =
            case build w of
                MkGetDiscardingUnlift getDiscardingUnlift' -> let
                    getDiscardingUnlift'' ::
                           forall m'. MonadTunnelIO m'
                        => StackT tt' m' (WMFunction (StackT tt' m') m')
                    getDiscardingUnlift'' =
                        MkStackT $
                        case witTransStackDict @MonadTunnelIO @tt0 @m' $ mapListType (\(Compose Dict) -> Compose Dict) w of
                            Dict ->
                                withTransConstraintTM @Monad $ do
                                    unlift1 <- getDiscardingUnlift
                                    unlift2 <- lift $ unStackT getDiscardingUnlift'
                                    return $ unlift2 . coerce unlift1
                    in MkGetDiscardingUnlift getDiscardingUnlift''
        in unGetDiscardingUnlift $ build $ representative @_ @(ListType (Compose Dict MonadTransUnlift)) @tt

stackLiftWithUnlift ::
       forall tt m. (MonadTransStackUnlift tt, MonadTunnelIO m)
    => MBackFunction m (ApplyStack tt m)
stackLiftWithUnlift = runWMBackFunction $ coerce $ MkWMBackFunction $ liftWithUnlift @(StackT tt) @m

type MonadTransStackUnliftAll tt
     = ( IsStack (TransConstraint MonadPlus) tt
       , IsStack MonadTransTunnel tt
       , MonadTransStackUnlift tt
       , IsStack MonadTransUnliftAll tt)

concatMonadTransStackUnliftAllDict ::
       forall tt1 tt2. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2)
    => Dict (MonadTransStackUnliftAll (Concat tt1 tt2))
concatMonadTransStackUnliftAllDict =
    case concatMonadTransStackUnliftDict @tt1 @tt2 of
        Dict ->
            case concatIsDict @(Compose Dict MonadTransUnliftAll) @tt1 @tt2 of
                Dict ->
                    case concatIsDict @(Compose Dict (TransConstraint MonadPlus)) @tt1 @tt2 of
                        Dict ->
                            case concatIsDict @(Compose Dict MonadTransTunnel) @tt1 @tt2 of
                                Dict -> Dict

newtype InsideOut t = MkInsideOut
    { unInsideOut :: forall m r.
                         Monad m => (forall b. (forall mm a. Monad mm => t mm a -> mm (a, b)) -> m (r, b)) -> t m r
    }

newtype LiftWithUnliftAll t = MkLiftWithUnliftAll
    { unLiftWithUnliftAll :: forall m r. MonadIO m => (UnliftAll MonadTunnelIO t -> m r) -> t m r
    }

newtype GetDiscardingUnliftAll t = MkGetDiscardingUnliftAll
    { unGetDiscardingUnliftAll :: forall m. Monad m => t m (WUnliftAll MonadTunnelIO t)
    }

stackJoinUnliftAll ::
       ListType (Compose Dict MonadTransUnliftAll) tt
    -> UnliftAll MonadTunnelIO t
    -> UnliftAll MonadTunnelIO (StackT tt)
    -> UnliftAll MonadTunnelIO (StackT (t ': tt))
stackJoinUnliftAll w tmm stmm (stma :: StackT (t ': tt) m a) =
    case witTransStackDict @MonadTunnelIO @tt @m $ mapListType (\(Compose Dict) -> Compose Dict) w of
        Dict -> stmm $ MkStackT $ tmm $ unStackT stma

stackTTransDict ::
       forall cm tt m a. cm m
    => ListType (Compose Dict (TransConstraint cm)) tt
    -> (cm (ApplyStack tt m) => StackT tt m a)
    -> StackT tt m a
stackTTransDict w r =
    case witTransStackDict @cm @tt @m w of
        Dict -> r

instance MonadTransStackUnliftAll tt => MonadTransUnliftAll (StackT tt) where
    insideOut ::
           forall m r. Monad m
        => (forall b. (forall mm a. Monad mm => StackT tt mm a -> mm (a, b)) -> m (r, b))
        -> StackT tt m r
    insideOut = let
        build :: forall tt'. ListType (Compose Dict MonadTransUnliftAll) tt' -> InsideOut (StackT tt')
        build NilListType = MkInsideOut $ \call -> MkStackT $ fmap fst $ call $ fmap (\a -> (a, ())) . unStackT
        build (ConsListType (Compose Dict) (w :: ListType _ tt0)) =
            case build w of
                MkInsideOut insideOut' -> let
                    w' = mapListType (\(Compose Dict) -> Compose Dict) w
                    insideOut'' ::
                           forall m' r'. Monad m'
                        => (forall b. (forall mm a. Monad mm => StackT tt' mm a -> mm (a, b)) -> m' (r', b))
                        -> StackT tt' m' r'
                    insideOut'' call =
                        case witTransStackDict @Monad @tt0 @m' w' of
                            Dict ->
                                MkStackT $
                                insideOut $ \unlift ->
                                    unStackT $
                                    insideOut' $ \unlift' ->
                                        fmap (\(r, (b, c)) -> ((r, b), c)) $
                                        call $ \stma ->
                                            fmap (\((a, b), c) -> (a, (b, c))) $
                                            unlift' $ stackTTransDict @Monad w' $ MkStackT $ unlift $ unStackT stma
                    in MkInsideOut insideOut''
        in unInsideOut $ build $ representative @_ @(ListType (Compose Dict MonadTransUnliftAll)) @tt
    liftWithUnliftAll ::
           forall m r. MonadIO m
        => (UnliftAll MonadTunnelIO (StackT tt) -> m r)
        -> StackT tt m r
    liftWithUnliftAll = let
        build :: forall tt'. ListType (Compose Dict MonadTransUnliftAll) tt' -> LiftWithUnliftAll (StackT tt')
        build NilListType = MkLiftWithUnliftAll $ \call -> MkStackT $ call unStackT
        build (ConsListType (Compose Dict) (w :: ListType _ tt0)) =
            case build w of
                MkLiftWithUnliftAll liftWithUnliftAll' -> let
                    liftWithUnliftAll'' ::
                           forall m' r'. MonadIO m'
                        => (UnliftAll MonadTunnelIO (StackT tt') -> m' r')
                        -> StackT tt' m' r'
                    liftWithUnliftAll'' call =
                        MkStackT $
                        case witTransStackDict @MonadIO @tt0 @m' $ mapListType (\(Compose Dict) -> Compose Dict) w of
                            Dict ->
                                liftWithUnliftAll $ \unlift1 ->
                                    unStackT $
                                    liftWithUnliftAll' $ \unlift2 -> call $ stackJoinUnliftAll w unlift1 unlift2
                    in MkLiftWithUnliftAll liftWithUnliftAll''
        in unLiftWithUnliftAll $ build $ representative @_ @(ListType (Compose Dict MonadTransUnliftAll)) @tt
    getDiscardingUnliftAll ::
           forall m. Monad m
        => StackT tt m (WUnliftAll MonadTunnelIO (StackT tt))
    getDiscardingUnliftAll = let
        build :: forall tt'. ListType (Compose Dict MonadTransUnliftAll) tt' -> GetDiscardingUnliftAll (StackT tt')
        build NilListType = MkGetDiscardingUnliftAll $ MkStackT $ return $ MkWUnliftAll unStackT
        build (ConsListType (Compose (Dict :: Dict (MonadTransUnliftAll t))) (w :: ListType _ tt0)) =
            case build w of
                MkGetDiscardingUnliftAll getDiscardingUnliftAll' -> let
                    getDiscardingUnliftAll'' ::
                           forall m'. Monad m'
                        => StackT tt' m' (WUnliftAll MonadTunnelIO (StackT tt'))
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

stackCommute ::
       forall tta ttb m r. (MonadTransStackUnliftAll tta, MonadTransStackUnliftAll ttb, MonadTunnelIO m)
    => ApplyStack tta (ApplyStack ttb m) r
    -> ApplyStack ttb (ApplyStack tta m) r
stackCommute aar =
    case (transStackDict @MonadTunnelIO @tta @m, transStackDict @MonadTunnelIO @ttb @m) of
        (Dict, Dict) -> let
            ssr :: StackT tta (StackT ttb m) r
            ssr = MkStackT $ stackRemonad @tta @(ApplyStack ttb m) @(StackT ttb m) MkStackT aar
            in stackRemonad @ttb @(StackT tta m) @(ApplyStack tta m) unStackT $ unStackT $ commuteT ssr

transStackConcatRefl ::
       forall (tt1 :: [TransKind]) (tt2 :: [TransKind]) m. MonadTransStackUnliftAll tt1
    => (ApplyStack (Concat tt1 tt2) m) :~: (ApplyStack tt1 (ApplyStack tt2 m))
transStackConcatRefl = applyConcatRefl @_ @tt1 @tt2 @m @(Compose Dict MonadTransUnliftAll)

type StackUnliftAll (tt :: [TransKind]) = forall m. MonadUnliftIO m => MFunction (ApplyStack tt m) m

newtype WStackUnliftAll (tt :: [TransKind]) = MkWStackUnliftAll
    { runWStackUnliftAll :: StackUnliftAll tt
    }

consWStackUnliftAll ::
       forall t tt. IsStack (TransConstraint MonadUnliftIO) tt
    => WUnliftAll MonadUnliftIO t
    -> WStackUnliftAll tt
    -> WStackUnliftAll (t ': tt)
consWStackUnliftAll (MkWUnliftAll unlift1) (MkWStackUnliftAll unliftr) = let
    unlift ::
           forall m. MonadUnliftIO m
        => MFunction (t (ApplyStack tt m)) m
    unlift =
        case transStackDict @MonadUnliftIO @tt @m of
            Dict -> unliftr . unlift1
    in MkWStackUnliftAll unlift

stackLiftWithUnliftAll ::
       forall tt m r. (MonadTransStackUnliftAll tt, MonadIO m)
    => (StackUnliftAll tt -> m r)
    -> ApplyStack tt m r
stackLiftWithUnliftAll call = unStackT @tt $ liftWithUnliftAll $ \unlift -> call $ \astm -> unlift $ MkStackT astm
