module Control.Monad.Trans.Stack
    ( TransKind
    , witTransStackDict
    , IsStack
    , transStackDict
    , StackT(..)
    , stackLift
    , stackHoist
    , MonadTransStackTunnel
    , transStackExcept
    , stackUnderliftIO
    , combineIOFunctions
    , stackLiftMBackFunction
    , MonadTransStackUnlift
    , concatMonadTransStackUnliftDict
    , stackLiftWithUnlift
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

type IsStack :: (TransKind -> Constraint) -> [TransKind] -> Constraint
type IsStack ct = Is (ListType (Compose Dict ct))

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
       , IsStack TransTunnel tt
       , IsStack MonadTrans tt
       , IsStack (WithTunnelConstraint FunctorOne) tt)

concatMonadTransStackTunnelDict ::
       forall tt1 tt2. (MonadTransStackTunnel tt1, MonadTransStackTunnel tt2)
    => Dict (MonadTransStackTunnel (Concat tt1 tt2))
concatMonadTransStackTunnelDict =
    case concatIsDict @(Compose Dict (TransConstraint Functor)) @tt1 @tt2 of
        Dict ->
            case concatIsDict @(Compose Dict (TransConstraint Monad)) @tt1 @tt2 of
                Dict ->
                    case concatIsDict @(Compose Dict TransTunnel) @tt1 @tt2 of
                        Dict ->
                            case concatIsDict @(Compose Dict MonadTrans) @tt1 @tt2 of
                                Dict ->
                                    case concatIsDict @(Compose Dict (WithTunnelConstraint FunctorOne)) @tt1 @tt2 of
                                        Dict -> Dict

newtype TunnelWrapper t = MkTunnel
    { unTunnel :: forall m2 r.
                      Functor m2 =>
                              ((forall m1 a. Functor m1 => t m1 a -> m1 (Tunnel t a)) -> m2 (Tunnel t r)) -> t m2 r
    }

type WithTunnelConstraint :: ((Type -> Type) -> Constraint) -> TransKind -> Constraint
class (TransTunnel t, c (Tunnel t)) => WithTunnelConstraint c t

instance (TransTunnel t, c (Tunnel t)) => WithTunnelConstraint c t

type ApplyStackTunnel :: [TransKind] -> Type -> Type
type family ApplyStackTunnel tt where
    ApplyStackTunnel '[] = Identity
    ApplyStackTunnel (t ': tt) = Compose (ApplyStackTunnel tt) (Tunnel t)

astIsWithTunnelConstraint ::
       forall (c :: (Type -> Type) -> Constraint) (tt :: [TransKind]).
       (c Identity, forall f1 f2. (c f1, c f2) => c (Compose f1 f2))
    => ListType (Compose Dict (WithTunnelConstraint c)) tt
    -> Dict (c (ApplyStackTunnel tt))
astIsWithTunnelConstraint NilListType = Dict
astIsWithTunnelConstraint (ConsListType (Compose Dict) w) =
    case astIsWithTunnelConstraint w of
        Dict -> Dict

isWithTunnelConstraint ::
       forall (c :: (Type -> Type) -> Constraint) (tt :: [TransKind]).
       (c Identity, forall f1 f2. (c f1, c f2) => c (Compose f1 f2))
    => IsStack (WithTunnelConstraint c) tt => Dict (c (ApplyStackTunnel tt))
isWithTunnelConstraint =
    astIsWithTunnelConstraint @c $ representative @_ @(ListType (Compose Dict (WithTunnelConstraint c))) @tt

type StackTunnel :: [TransKind] -> Type -> Type
newtype StackTunnel tt a = MkStackTunnel
    { unStackTunnel :: ApplyStackTunnel tt a
    }

instance IsStack (WithTunnelConstraint FunctorOne) tt => Functor (StackTunnel tt) where
    fmap =
        case isWithTunnelConstraint @FunctorOne @tt of
            Dict -> \ab (MkStackTunnel st) -> MkStackTunnel $ fmap ab st

instance IsStack (WithTunnelConstraint FunctorOne) tt => FunctorOne (StackTunnel tt) where
    fpure =
        case isWithTunnelConstraint @FunctorOne @tt of
            Dict -> \a -> MkStackTunnel $ fpure a
    getMaybeOne =
        case isWithTunnelConstraint @FunctorOne @tt of
            Dict -> \(MkStackTunnel st) -> getMaybeOne st

instance ( IsStack (WithTunnelConstraint FunctorOne) tt
         , IsStack (WithTunnelConstraint FunctorExtract) tt
         , IsStack MonadTransUnlift tt
         ) => FunctorExtract (StackTunnel tt) where
    fextract =
        case isWithTunnelConstraint @FunctorExtract @tt of
            Dict -> \(MkStackTunnel st) -> fextract st

instance MonadTransStackTunnel tt => TransTunnel (StackT tt) where
    type Tunnel (StackT tt) = StackTunnel tt
    tunnel ::
           forall m2 r. Functor m2
        => ((forall m1 a. Functor m1 => StackT tt m1 a -> m1 (Tunnel (StackT tt) a)) -> m2 (Tunnel (StackT tt) r))
        -> StackT tt m2 r
    tunnel = let
        build :: forall tt'. ListType (Compose Dict TransTunnel) tt' -> TunnelWrapper (StackT tt')
        build NilListType =
            MkTunnel $ \call ->
                MkStackT $ fmap (runIdentity . unStackTunnel) $ call $ fmap (MkStackTunnel . Identity) . unStackT
        build (ConsListType (Compose Dict) (w :: ListType _ tt0)) =
            case build w of
                MkTunnel tunnel' -> let
                    tunnel'' ::
                           forall m2' r'. Functor m2'
                        => ((forall m1 a. Functor m1 => StackT tt' m1 a -> m1 (Tunnel (StackT tt') a)) -> m2' (Tunnel (StackT tt') r'))
                        -> StackT tt' m2' r'
                    tunnel'' call =
                        case (witTransStackDict @Functor @tt0 @m2' $ mapListType (\(Compose Dict) -> Compose Dict) w) of
                            Dict ->
                                MkStackT $
                                tunnel $ \unlift1 ->
                                    unStackT $
                                    tunnel' $ \unlift2 ->
                                        fmap (MkStackTunnel . getCompose . unStackTunnel) $
                                        call $ \(MkStackT stt :: _ m1 _) ->
                                            case (witTransStackDict @Functor @tt0 @m1 $
                                                  mapListType (\(Compose Dict) -> Compose Dict) w) of
                                                Dict ->
                                                    fmap (MkStackTunnel . Compose . unStackTunnel) $
                                                    unlift2 $ MkStackT $ unlift1 stt
                    in MkTunnel tunnel''
        in unTunnel $ build $ representative @_ @(ListType (Compose Dict TransTunnel)) @tt

stackHoist ::
       forall tt ma mb. (MonadTransStackTunnel tt, Monad ma, Monad mb)
    => MFunction ma mb
    -> MFunction (ApplyStack tt ma) (ApplyStack tt mb)
stackHoist mf asta = unStackT $ hoist mf $ MkStackT @tt asta

transStackExcept ::
       forall tt m e a. (MonadTransStackTunnel tt, Monad m)
    => ApplyStack tt (ExceptT e m) a
    -> ApplyStack tt m (Either e a)
transStackExcept ata = unStackT $ transExcept $ MkStackT @tt @(ExceptT e m) ata

stackUnderliftIO ::
       forall tt m. (MonadTransStackTunnel tt, MonadIO m)
    => MFunction (ApplyStack tt IO) (ApplyStack tt m)
stackUnderliftIO = stackHoist @tt @IO @m liftIO

combineIOFunctions ::
       forall tt m. (MonadTransStackTunnel tt, Monad m)
    => IOFunction (ApplyStack tt IO)
    -> IOFunction m
    -> IOFunction (ApplyStack tt m)
combineIOFunctions fa fb = runWMFunction $ MkWMFunction fa . MkWMFunction (stackHoist @tt fb)

stackLiftWMBackFunction ::
       forall tt ma mb. (MonadTransStackUnlift tt, Monad ma, Monad mb)
    => WMBackFunction ma mb
    -> WMBackFunction (ApplyStack tt ma) (ApplyStack tt mb)
stackLiftWMBackFunction mab = coerce $ liftWMBackFunction @(StackT tt) mab

stackLiftMBackFunction ::
       forall tt ma mb. (MonadTransStackUnlift tt, Monad ma, Monad mb)
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
       , IsStack (WithTunnelConstraint FunctorExtract) tt
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
                                                    case concatIsDict @(Compose Dict TransTunnel) @tt1 @tt2 of
                                                        Dict ->
                                                            case concatIsDict @(Compose Dict MonadTransUnlift) @tt1 @tt2 of
                                                                Dict ->
                                                                    case concatIsDict
                                                                             @(Compose Dict (WithTunnelConstraint FunctorExtract))
                                                                             @tt1
                                                                             @tt2 of
                                                                        Dict -> Dict

stackLiftWithUnlift ::
       forall tt m. (MonadTransStackUnlift tt, MonadTunnelIO m)
    => MBackFunction m (ApplyStack tt m)
stackLiftWithUnlift = runWMBackFunction $ coerce $ MkWMBackFunction $ liftWithUnlift @(StackT tt) @m

concatMonadTransStackUnliftAllDict ::
       forall tt1 tt2. (MonadTransStackUnlift tt1, MonadTransStackUnlift tt2)
    => Dict (MonadTransStackUnlift (Concat tt1 tt2))
concatMonadTransStackUnliftAllDict =
    case concatMonadTransStackUnliftDict @tt1 @tt2 of
        Dict ->
            case concatIsDict @(Compose Dict MonadTransUnlift) @tt1 @tt2 of
                Dict ->
                    case concatIsDict @(Compose Dict TransTunnel) @tt1 @tt2 of
                        Dict -> Dict

newtype LiftWithUnliftAll t = MkLiftWithUnliftAll
    { unLiftWithUnliftAll :: forall m r. MonadIO m => (UnliftT MonadTunnelIO t -> m r) -> t m r
    }

witFunctorOneTunnelIOStackDict ::
       forall tt m. FunctorOne (TunnelIO m)
    => ListType (Compose Dict MonadTransUnlift) tt
    -> Dict (FunctorOne (TunnelIO (ApplyStack tt m)))
witFunctorOneTunnelIOStackDict NilListType = Dict
witFunctorOneTunnelIOStackDict (ConsListType (Compose Dict) lt) =
    case witFunctorOneTunnelIOStackDict @_ @m lt of
        Dict -> Dict

stackJoinUnliftAll ::
       ListType (Compose Dict MonadTransUnlift) tt
    -> UnliftT MonadTunnelIO t
    -> UnliftT MonadTunnelIO (StackT tt)
    -> UnliftT MonadTunnelIO (StackT (t ': tt))
stackJoinUnliftAll w tmm stmm (stma :: StackT (t ': tt) m a) =
    case witTransStackDict @MonadTunnelIO @tt @m $ mapListType (\(Compose Dict) -> Compose Dict) w of
        Dict ->
            case witFunctorOneTunnelIOStackDict @tt @m w of
                Dict -> stmm $ MkStackT $ tmm $ unStackT stma

newtype GetDiscardingUnlift t = MkGetDiscardingUnlift
    { unGetDiscardingUnlift :: forall m. Monad m => t m (WUnliftT MonadTunnelIO t)
    }

instance MonadTransStackUnlift tt => MonadTransUnlift (StackT tt) where
    liftWithUnlift ::
           forall m r. MonadIO m
        => (UnliftT MonadTunnelIO (StackT tt) -> m r)
        -> StackT tt m r
    liftWithUnlift = let
        build :: forall tt'. ListType (Compose Dict MonadTransUnlift) tt' -> LiftWithUnliftAll (StackT tt')
        build NilListType = MkLiftWithUnliftAll $ \call -> MkStackT $ call unStackT
        build (ConsListType (Compose Dict) (w :: ListType _ tt0)) =
            case build w of
                MkLiftWithUnliftAll liftWithUnlift' -> let
                    liftWithUnlift'' ::
                           forall m' r'. MonadIO m'
                        => (UnliftT MonadTunnelIO (StackT tt') -> m' r')
                        -> StackT tt' m' r'
                    liftWithUnlift'' call =
                        MkStackT $
                        case witTransStackDict @MonadIO @tt0 @m' $ mapListType (\(Compose Dict) -> Compose Dict) w of
                            Dict ->
                                liftWithUnlift $ \unlift1 ->
                                    unStackT $ liftWithUnlift' $ \unlift2 -> call $ stackJoinUnliftAll w unlift1 unlift2
                    in MkLiftWithUnliftAll liftWithUnlift''
        in unLiftWithUnliftAll $ build $ representative @_ @(ListType (Compose Dict MonadTransUnlift)) @tt
    getDiscardingUnlift ::
           forall m. Monad m
        => StackT tt m (WUnliftT MonadTunnelIO (StackT tt))
    getDiscardingUnlift = let
        build :: forall tt'. ListType (Compose Dict MonadTransUnlift) tt' -> GetDiscardingUnlift (StackT tt')
        build NilListType = MkGetDiscardingUnlift $ MkStackT $ return $ MkWUnliftT unStackT
        build (ConsListType (Compose (Dict :: Dict (MonadTransUnlift t))) (w :: ListType _ tt0)) =
            case build w of
                MkGetDiscardingUnlift getDiscardingUnlift' -> let
                    getDiscardingUnlift'' ::
                           forall m'. Monad m'
                        => StackT tt' m' (WUnliftT MonadTunnelIO (StackT tt'))
                    getDiscardingUnlift'' =
                        MkStackT $
                        case witTransStackDict @Monad @tt0 @m' $ mapListType (\(Compose Dict) -> Compose Dict) w of
                            Dict ->
                                withTransConstraintTM @Monad $ do
                                    MkWUnliftT unlift1 <- getDiscardingUnlift
                                    MkWUnliftT unlift2 <- lift $ unStackT $ getDiscardingUnlift' @m'
                                    return $ MkWUnliftT $ stackJoinUnliftAll w unlift1 unlift2
                    in MkGetDiscardingUnlift getDiscardingUnlift''
        in unGetDiscardingUnlift $ build $ representative @_ @(ListType (Compose Dict MonadTransUnlift)) @tt

concatFstMFunction ::
       forall tt1 tt2 m. (MonadTransStackUnlift tt1, MonadTransStackUnlift tt2, Monad m)
    => MFunction (ApplyStack tt1 m) (ApplyStack (Concat tt1 tt2) m)
concatFstMFunction =
    case transStackConcatRefl @tt1 @tt2 @m of
        Refl ->
            case transStackDict @Monad @tt2 @m of
                Dict -> stackHoist @tt1 $ stackLift @tt2 @m

concatSndMFunction ::
       forall tt1 tt2 m. (MonadTransStackUnlift tt1, MonadTransStackUnlift tt2, Monad m)
    => MFunction (ApplyStack tt2 m) (ApplyStack (Concat tt1 tt2) m)
concatSndMFunction =
    case transStackConcatRefl @tt1 @tt2 @m of
        Refl ->
            case transStackDict @Monad @tt2 @m of
                Dict -> stackLift @tt1

stackCommute ::
       forall tta ttb m r. (MonadTransStackUnlift tta, MonadTransStackUnlift ttb, MonadTunnelIO m)
    => ApplyStack tta (ApplyStack ttb m) r
    -> ApplyStack ttb (ApplyStack tta m) r
stackCommute aar =
    case (transStackDict @MonadTunnelIO @tta @m, transStackDict @MonadTunnelIO @ttb @m) of
        (Dict, Dict) -> let
            ssr :: StackT tta (StackT ttb m) r
            ssr = MkStackT $ stackHoist @tta @(ApplyStack ttb m) @(StackT ttb m) MkStackT aar
            in stackHoist @ttb @(StackT tta m) @(ApplyStack tta m) unStackT $ unStackT $ commuteT ssr

transStackConcatRefl ::
       forall (tt1 :: [TransKind]) (tt2 :: [TransKind]) m. MonadTransStackUnlift tt1
    => (ApplyStack (Concat tt1 tt2) m) :~: (ApplyStack tt1 (ApplyStack tt2 m))
transStackConcatRefl = applyConcatRefl @_ @tt1 @tt2 @m @(Compose Dict MonadTransUnlift)

type StackUnliftAll (tt :: [TransKind]) = forall m. MonadUnliftIO m => MFunction (ApplyStack tt m) m

newtype WStackUnliftAll (tt :: [TransKind]) = MkWStackUnliftAll
    { runWStackUnliftAll :: StackUnliftAll tt
    }

consWStackUnliftAll ::
       forall t tt. IsStack (TransConstraint MonadUnliftIO) tt
    => WUnliftT MonadUnliftIO t
    -> WStackUnliftAll tt
    -> WStackUnliftAll (t ': tt)
consWStackUnliftAll (MkWUnliftT unlift1) (MkWStackUnliftAll unliftr) = let
    unlift ::
           forall m. MonadUnliftIO m
        => MFunction (t (ApplyStack tt m)) m
    unlift =
        case transStackDict @MonadUnliftIO @tt @m of
            Dict -> unliftr . unlift1
    in MkWStackUnliftAll unlift

stackLiftWithUnliftAll ::
       forall tt m r. (MonadTransStackUnlift tt, MonadIO m)
    => (StackUnliftAll tt -> m r)
    -> ApplyStack tt m r
stackLiftWithUnliftAll call = unStackT @tt $ liftWithUnlift $ \unlift -> call $ \astm -> unlift $ MkStackT astm
