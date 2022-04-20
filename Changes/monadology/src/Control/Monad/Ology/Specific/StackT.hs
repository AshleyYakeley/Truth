module Control.Monad.Ology.Specific.StackT
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

import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.ComposeInner
import Control.Monad.Ology.Specific.ExceptT
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

type StackT :: [TransKind] -> TransKind
newtype StackT tt m a = MkStackT
    { unStackT :: ApplyStack tt m a
    }

instance (IsStack (TransConstraint Functor) tt, Functor m) => Functor (StackT tt m) where
    fmap =
        case transStackDict @Functor @tt @m of
            Dict -> \ab (MkStackT ma) -> MkStackT $ fmap ab ma

instance (IsStack (TransConstraint Functor) tt) => TransConstraint Functor (StackT tt) where
    hasTransConstraint = Dict

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

instance (IsStack (TransConstraint Functor) tt, IsStack (TransConstraint Monad) tt) => TransConstraint Monad (StackT tt) where
    hasTransConstraint = Dict

instance (IsStack (TransConstraint Functor) tt, IsStack (TransConstraint Monad) tt, IsStack MonadTrans tt, MonadFail m) =>
             MonadFail (StackT tt m) where
    fail s = lift $ fail s

instance ( IsStack (TransConstraint Functor) tt
         , IsStack (TransConstraint Monad) tt
         , IsStack (TransConstraint MonadFail) tt
         , IsStack MonadTrans tt
         ) => TransConstraint MonadFail (StackT tt) where
    hasTransConstraint = Dict

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
         , IsStack (TransConstraint MonadFix) tt
         ) => TransConstraint MonadFix (StackT tt) where
    hasTransConstraint = Dict

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
         , IsStack (TransConstraint MonadIO) tt
         ) => TransConstraint MonadIO (StackT tt) where
    hasTransConstraint = Dict

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

instance ( IsStack (TransConstraint Functor) tt
         , IsStack (TransConstraint Monad) tt
         , IsStack (TransConstraint MonadPlus) tt
         ) => TransConstraint MonadPlus (StackT tt) where
    hasTransConstraint = Dict

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

stackLift ::
       forall tt m.
       (IsStack (TransConstraint Functor) tt, IsStack (TransConstraint Monad) tt, IsStack MonadTrans tt, Monad m)
    => m --> ApplyStack tt m
stackLift ma = unStackT @tt @m $ lift ma

type MonadTransStackTunnel tt
     = ( IsStack (TransConstraint Functor) tt
       , IsStack (TransConstraint Monad) tt
       , IsStack MonadTransTunnel tt
       , IsStack MonadTrans tt
       , IsStack (WithTunnelConstraint Functor) tt
       , IsStack (WithTunnelConstraint Applicative) tt
       , IsStack (WithTunnelConstraint Monad) tt
       , IsStack (WithTunnelConstraint Traversable) tt
       , IsStack (WithTunnelConstraint MonadInner) tt)

concatMonadTransStackTunnelDict ::
       forall tt1 tt2. (MonadTransStackTunnel tt1, MonadTransStackTunnel tt2)
    => Dict (MonadTransStackTunnel (Concat tt1 tt2))
concatMonadTransStackTunnelDict =
    withConcatIs @_ @(Compose Dict (TransConstraint Functor)) @tt1 @tt2 $
    withConcatIs @_ @(Compose Dict (TransConstraint Monad)) @tt1 @tt2 $
    withConcatIs @_ @(Compose Dict MonadTransTunnel) @tt1 @tt2 $
    withConcatIs @_ @(Compose Dict MonadTrans) @tt1 @tt2 $
    withConcatIs @_ @(Compose Dict (WithTunnelConstraint Functor)) @tt1 @tt2 $
    withConcatIs @_ @(Compose Dict (WithTunnelConstraint Applicative)) @tt1 @tt2 $
    withConcatIs @_ @(Compose Dict (WithTunnelConstraint Monad)) @tt1 @tt2 $
    withConcatIs @_ @(Compose Dict (WithTunnelConstraint Traversable)) @tt1 @tt2 $
    withConcatIs @_ @(Compose Dict (WithTunnelConstraint MonadInner)) @tt1 @tt2 $ Dict

newtype TunnelWrapper t = MkTunnel
    { unTunnel :: forall m2 r.
                      Functor m2 =>
                              ((forall m1 a. Functor m1 => t m1 a -> m1 (Tunnel t a)) -> m2 (Tunnel t r)) -> t m2 r
    }

type WithTunnelConstraint :: ((Type -> Type) -> Constraint) -> TransKind -> Constraint
class (MonadTransTunnel t, c (Tunnel t)) => WithTunnelConstraint c t

instance (MonadTransTunnel t, c (Tunnel t)) => WithTunnelConstraint c t

type ApplyStackTunnel :: [TransKind] -> Type -> Type
type family ApplyStackTunnel tt where
    ApplyStackTunnel '[] = Identity
    ApplyStackTunnel (t ': tt) = ComposeInner (Tunnel t) (ApplyStackTunnel tt)

astIsWithTunnelConstraint ::
       forall (c :: (Type -> Type) -> Constraint) (tt :: [TransKind]).
       (c Identity, forall f1 f2. (c f1, c f2) => c (ComposeInner f1 f2))
    => ListType (Compose Dict (WithTunnelConstraint c)) tt
    -> Dict (c (ApplyStackTunnel tt))
astIsWithTunnelConstraint NilListType = Dict
astIsWithTunnelConstraint (ConsListType (Compose Dict) w) =
    case astIsWithTunnelConstraint w of
        Dict -> Dict

isWithTunnelConstraint ::
       forall (c :: (Type -> Type) -> Constraint) (tt :: [TransKind]).
       (c Identity, forall f1 f2. (c f1, c f2) => c (ComposeInner f1 f2))
    => IsStack (WithTunnelConstraint c) tt => Dict (c (ApplyStackTunnel tt))
isWithTunnelConstraint =
    astIsWithTunnelConstraint @c $ representative @_ @(ListType (Compose Dict (WithTunnelConstraint c))) @tt

type StackTunnel :: [TransKind] -> Type -> Type
newtype StackTunnel tt a = MkStackTunnel
    { unStackTunnel :: ApplyStackTunnel tt a
    }

instance IsStack (WithTunnelConstraint Functor) tt => Functor (StackTunnel tt) where
    fmap =
        case isWithTunnelConstraint @Functor @tt of
            Dict -> \ab (MkStackTunnel st) -> MkStackTunnel $ fmap ab st

instance (IsStack (WithTunnelConstraint Functor) tt, IsStack (WithTunnelConstraint MonadInner) tt) =>
             Applicative (StackTunnel tt) where
    pure =
        case isWithTunnelConstraint @MonadInner @tt of
            Dict -> \a -> MkStackTunnel $ pure a
    (<*>) =
        case isWithTunnelConstraint @MonadInner @tt of
            Dict -> \(MkStackTunnel mab) (MkStackTunnel ma) -> MkStackTunnel $ mab <*> ma

instance (IsStack (WithTunnelConstraint Functor) tt, IsStack (WithTunnelConstraint MonadInner) tt) =>
             Foldable (StackTunnel tt) where
    foldMap =
        case isWithTunnelConstraint @MonadInner @tt of
            Dict -> \ab (MkStackTunnel st) -> foldMap ab st

instance (IsStack (WithTunnelConstraint Functor) tt, IsStack (WithTunnelConstraint MonadInner) tt) =>
             Traversable (StackTunnel tt) where
    traverse =
        case isWithTunnelConstraint @MonadInner @tt of
            Dict -> \ab (MkStackTunnel st) -> MkStackTunnel <$> traverse ab st

instance (IsStack (WithTunnelConstraint Functor) tt, IsStack (WithTunnelConstraint MonadInner) tt) =>
             Monad (StackTunnel tt) where
    return = pure
    (>>=) =
        case isWithTunnelConstraint @MonadInner @tt of
            Dict -> \(MkStackTunnel ma) q -> MkStackTunnel $ ma >>= unStackTunnel . q

instance (IsStack (WithTunnelConstraint Functor) tt, IsStack (WithTunnelConstraint MonadInner) tt) =>
             MonadException (StackTunnel tt) where
    type Exc (StackTunnel tt) = Exc (ApplyStackTunnel tt)
    throwExc e =
        case isWithTunnelConstraint @MonadInner @tt of
            Dict -> MkStackTunnel $ throwExc e
    catchExc (MkStackTunnel st) q =
        case isWithTunnelConstraint @MonadInner @tt of
            Dict -> MkStackTunnel $ catchExc st (unStackTunnel . q)

instance (IsStack (WithTunnelConstraint Functor) tt, IsStack (WithTunnelConstraint MonadInner) tt) =>
             MonadInner (StackTunnel tt) where
    retrieveInner =
        case isWithTunnelConstraint @MonadInner @tt of
            Dict -> \(MkStackTunnel st) -> retrieveInner st

instance ( IsStack (WithTunnelConstraint Functor) tt
         , IsStack (WithTunnelConstraint MonadInner) tt
         , IsStack (WithTunnelConstraint MonadExtract) tt
         , IsStack MonadTransUnlift tt
         ) => MonadExtract (StackTunnel tt) where
    mToValue =
        case isWithTunnelConstraint @MonadExtract @tt of
            Dict -> \(MkStackTunnel st) -> mToValue st

instance MonadTransStackTunnel tt => MonadTransTunnel (StackT tt) where
    type Tunnel (StackT tt) = StackTunnel tt
    tunnel ::
           forall m2 r. Functor m2
        => ((forall m1 a. Functor m1 => StackT tt m1 a -> m1 (Tunnel (StackT tt) a)) -> m2 (Tunnel (StackT tt) r))
        -> StackT tt m2 r
    tunnel = let
        build :: forall tt'. ListType (Compose Dict MonadTransTunnel) tt' -> TunnelWrapper (StackT tt')
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
                                        fmap (MkStackTunnel . getComposeInner . unStackTunnel) $
                                        call $ \(MkStackT stt :: _ m1 _) ->
                                            case (witTransStackDict @Functor @tt0 @m1 $
                                                  mapListType (\(Compose Dict) -> Compose Dict) w) of
                                                Dict ->
                                                    fmap (MkStackTunnel . MkComposeInner . unStackTunnel) $
                                                    unlift2 $ MkStackT $ unlift1 stt
                    in MkTunnel tunnel''
        in unTunnel $ build $ representative @_ @(ListType (Compose Dict MonadTransTunnel)) @tt

stackHoist ::
       forall tt ma mb. (MonadTransStackTunnel tt, Monad ma, Monad mb)
    => (ma --> mb)
    -> ApplyStack tt ma --> ApplyStack tt mb
stackHoist mf asta = unStackT $ hoist mf $ MkStackT @tt asta

transStackExcept ::
       forall tt m e a. (MonadTransStackTunnel tt, Monad m)
    => ApplyStack tt (ExceptT e m) a
    -> ApplyStack tt m (Either e a)
transStackExcept ata = unStackT $ transExcept $ MkStackT @tt @(ExceptT e m) ata

stackUnderliftIO ::
       forall tt m. (MonadTransStackTunnel tt, MonadIO m)
    => ApplyStack tt IO --> ApplyStack tt m
stackUnderliftIO = stackHoist @tt @IO @m liftIO

combineIOFunctions ::
       forall tt m. (MonadTransStackTunnel tt, Monad m)
    => (ApplyStack tt IO --> IO)
    -> (m --> IO)
    -> ApplyStack tt m --> IO
combineIOFunctions fa fb = runWMFunction $ MkWMFunction fa . MkWMFunction (stackHoist @tt fb)

stackLiftWMBackFunction ::
       forall tt ma mb. (MonadTransStackUnlift tt, Monad ma, Monad mb)
    => WMBackFunction ma mb
    -> WMBackFunction (ApplyStack tt ma) (ApplyStack tt mb)
stackLiftWMBackFunction mab = coerce $ backHoistW @(StackT tt) mab

stackLiftMBackFunction ::
       forall tt ma mb. (MonadTransStackUnlift tt, Monad ma, Monad mb)
    => (ma -/-> mb)
    -> ApplyStack tt ma -/-> ApplyStack tt mb
stackLiftMBackFunction f = runWMBackFunction $ stackLiftWMBackFunction @tt $ MkWMBackFunction f

type MonadTransStackUnlift tt
     = ( IsStack (TransConstraint MonadFail) tt
       , IsStack (TransConstraint MonadIO) tt
       , IsStack (TransConstraint MonadFix) tt
       , IsStack (TransConstraint MonadTunnelIO) tt
       , IsStack (TransConstraint MonadUnliftIO) tt
       , MonadTransStackTunnel tt
       , IsStack (WithTunnelConstraint MonadExtract) tt
       , IsStack MonadTransUnlift tt)

concatMonadTransStackUnliftDict ::
       forall tt1 tt2. (MonadTransStackUnlift tt1, MonadTransStackUnlift tt2)
    => Dict (MonadTransStackUnlift (Concat tt1 tt2))
concatMonadTransStackUnliftDict =
    case concatIsDict @_ @(Compose Dict (TransConstraint MonadFail)) @tt1 @tt2 of
        Dict ->
            case concatIsDict @_ @(Compose Dict (TransConstraint MonadIO)) @tt1 @tt2 of
                Dict ->
                    case concatIsDict @_ @(Compose Dict (TransConstraint MonadFix)) @tt1 @tt2 of
                        Dict ->
                            case concatIsDict @_ @(Compose Dict (TransConstraint MonadTunnelIO)) @tt1 @tt2 of
                                Dict ->
                                    case concatIsDict @_ @(Compose Dict (TransConstraint MonadUnliftIO)) @tt1 @tt2 of
                                        Dict ->
                                            case concatMonadTransStackTunnelDict @tt1 @tt2 of
                                                Dict ->
                                                    case concatIsDict @_ @(Compose Dict MonadTransTunnel) @tt1 @tt2 of
                                                        Dict ->
                                                            case concatIsDict
                                                                     @_
                                                                     @(Compose Dict MonadTransUnlift)
                                                                     @tt1
                                                                     @tt2 of
                                                                Dict ->
                                                                    case concatIsDict
                                                                             @_
                                                                             @(Compose Dict (WithTunnelConstraint MonadExtract))
                                                                             @tt1
                                                                             @tt2 of
                                                                        Dict -> Dict

stackLiftWithUnlift ::
       forall tt m. (MonadTransStackUnlift tt, MonadTunnelIO m)
    => m -/-> ApplyStack tt m
stackLiftWithUnlift = runWMBackFunction $ coerce $ liftWithUnliftW @(StackT tt) @m

concatMonadTransStackUnliftAllDict ::
       forall tt1 tt2. (MonadTransStackUnlift tt1, MonadTransStackUnlift tt2)
    => Dict (MonadTransStackUnlift (Concat tt1 tt2))
concatMonadTransStackUnliftAllDict =
    case concatMonadTransStackUnliftDict @tt1 @tt2 of
        Dict ->
            case concatIsDict @_ @(Compose Dict MonadTransUnlift) @tt1 @tt2 of
                Dict ->
                    case concatIsDict @_ @(Compose Dict MonadTransTunnel) @tt1 @tt2 of
                        Dict -> Dict

newtype LiftWithUnliftAll t = MkLiftWithUnliftAll
    { unLiftWithUnliftAll :: forall m r. MonadIO m => (Unlift MonadTunnelIO t -> m r) -> t m r
    }

witFunctorOneTunnelIOStackDict ::
       forall tt m. MonadInner (TunnelIO m)
    => ListType (Compose Dict MonadTransUnlift) tt
    -> Dict (MonadInner (TunnelIO (ApplyStack tt m)))
witFunctorOneTunnelIOStackDict NilListType = Dict
witFunctorOneTunnelIOStackDict (ConsListType (Compose Dict) lt) =
    case witFunctorOneTunnelIOStackDict @_ @m lt of
        Dict -> Dict

stackJoinUnliftAll ::
       ListType (Compose Dict MonadTransUnlift) tt
    -> Unlift MonadTunnelIO t
    -> Unlift MonadTunnelIO (StackT tt)
    -> Unlift MonadTunnelIO (StackT (t ': tt))
stackJoinUnliftAll w tmm stmm (stma :: StackT (t ': tt) m a) =
    case witTransStackDict @MonadTunnelIO @tt @m $ mapListType (\(Compose Dict) -> Compose Dict) w of
        Dict ->
            case witFunctorOneTunnelIOStackDict @tt @m w of
                Dict -> stmm $ MkStackT $ tmm $ unStackT stma

newtype GetDiscardingUnlift t = MkGetDiscardingUnlift
    { unGetDiscardingUnlift :: forall m. Monad m => t m (WUnlift MonadTunnelIO t)
    }

instance MonadTransStackUnlift tt => MonadTransUnlift (StackT tt) where
    liftWithUnlift ::
           forall m r. MonadIO m
        => (Unlift MonadTunnelIO (StackT tt) -> m r)
        -> StackT tt m r
    liftWithUnlift = let
        build :: forall tt'. ListType (Compose Dict MonadTransUnlift) tt' -> LiftWithUnliftAll (StackT tt')
        build NilListType = MkLiftWithUnliftAll $ \call -> MkStackT $ call unStackT
        build (ConsListType (Compose Dict) (w :: ListType _ tt0)) =
            case build w of
                MkLiftWithUnliftAll liftWithUnlift' -> let
                    liftWithUnlift'' ::
                           forall m' r'. MonadIO m'
                        => (Unlift MonadTunnelIO (StackT tt') -> m' r')
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
        => StackT tt m (WUnlift MonadTunnelIO (StackT tt))
    getDiscardingUnlift = let
        build :: forall tt'. ListType (Compose Dict MonadTransUnlift) tt' -> GetDiscardingUnlift (StackT tt')
        build NilListType = MkGetDiscardingUnlift $ MkStackT $ return $ MkWUnlift unStackT
        build (ConsListType (Compose (Dict :: Dict (MonadTransUnlift t))) (w :: ListType _ tt0)) =
            case build w of
                MkGetDiscardingUnlift getDiscardingUnlift' -> let
                    getDiscardingUnlift'' ::
                           forall m'. Monad m'
                        => StackT tt' m' (WUnlift MonadTunnelIO (StackT tt'))
                    getDiscardingUnlift'' =
                        MkStackT $
                        case witTransStackDict @Monad @tt0 @m' $ mapListType (\(Compose Dict) -> Compose Dict) w of
                            Dict ->
                                withTransConstraintTM @Monad $ do
                                    MkWUnlift unlift1 <- getDiscardingUnlift
                                    MkWUnlift unlift2 <- lift $ unStackT $ getDiscardingUnlift' @m'
                                    return $ MkWUnlift $ stackJoinUnliftAll w unlift1 unlift2
                    in MkGetDiscardingUnlift getDiscardingUnlift''
        in unGetDiscardingUnlift $ build $ representative @_ @(ListType (Compose Dict MonadTransUnlift)) @tt

concatFstMFunction ::
       forall tt1 tt2 m. (MonadTransStackUnlift tt1, MonadTransStackUnlift tt2, Monad m)
    => ApplyStack tt1 m --> ApplyStack (Concat tt1 tt2) m
concatFstMFunction =
    case transStackConcatRefl @tt1 @tt2 @m of
        Refl ->
            case transStackDict @Monad @tt2 @m of
                Dict -> stackHoist @tt1 $ stackLift @tt2 @m

concatSndMFunction ::
       forall tt1 tt2 m. (MonadTransStackUnlift tt1, MonadTransStackUnlift tt2, Monad m)
    => ApplyStack tt2 m --> ApplyStack (Concat tt1 tt2) m
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

type StackUnliftAll (tt :: [TransKind]) = forall m. MonadUnliftIO m => ApplyStack tt m --> m

newtype WStackUnliftAll (tt :: [TransKind]) = MkWStackUnliftAll
    { runWStackUnliftAll :: StackUnliftAll tt
    }

consWStackUnliftAll ::
       forall t tt. IsStack (TransConstraint MonadUnliftIO) tt
    => WUnlift MonadUnliftIO t
    -> WStackUnliftAll tt
    -> WStackUnliftAll (t ': tt)
consWStackUnliftAll (MkWUnlift unlift1) (MkWStackUnliftAll unliftr) = let
    unlift ::
           forall m. MonadUnliftIO m
        => t (ApplyStack tt m) --> m
    unlift =
        case transStackDict @MonadUnliftIO @tt @m of
            Dict -> unliftr . unlift1
    in MkWStackUnliftAll unlift

stackLiftWithUnliftAll ::
       forall tt m r. (MonadTransStackUnlift tt, MonadIO m)
    => (StackUnliftAll tt -> m r)
    -> ApplyStack tt m r
stackLiftWithUnliftAll call = unStackT @tt $ liftWithUnlift $ \unlift -> call $ \astm -> unlift $ MkStackT astm
