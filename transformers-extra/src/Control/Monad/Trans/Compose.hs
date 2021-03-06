module Control.Monad.Trans.Compose where

import Control.Monad.Trans.AskUnlift
import Control.Monad.Trans.Coerce
import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Function
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Unlift
import Import

newtype ComposeT (t1 :: (Type -> Type) -> (Type -> Type)) (t2 :: (Type -> Type) -> (Type -> Type)) (m :: Type -> Type) (a :: Type) = MkComposeT
    { unComposeT :: t1 (t2 m) a
    } deriving (Functor, Applicative, Alternative, Monad, MonadFail, MonadIO, MonadFix, MonadPlus)

lift1ComposeT ::
       forall t1 t2 m a. (MonadTransSemiTunnel t1, MonadTransConstraint Monad t2, Monad m)
    => t1 m a
    -> ComposeT t1 t2 m a
lift1ComposeT t1ma =
    case hasTransConstraint @Monad @t2 @m of
        Dict -> MkComposeT $ remonad lift t1ma

lift2ComposeT ::
       forall t1 t2 m a. (MonadTrans t1, Monad (t2 m))
    => t2 m a
    -> ComposeT t1 t2 m a
lift2ComposeT t2ma = MkComposeT $ lift t2ma

lift2ComposeT' ::
       forall t1 t2 m a. (MonadTrans t1, MonadTransConstraint Monad t2, Monad m)
    => t2 m a
    -> ComposeT t1 t2 m a
lift2ComposeT' =
    case hasTransConstraint @Monad @t2 @m of
        Dict -> lift2ComposeT

lift2ComposeT'' ::
       forall t1 t2 m a. (MonadTrans t1, MonadTransConstraint MonadIO t2, MonadIO m)
    => t2 m a
    -> ComposeT t1 t2 m a
lift2ComposeT'' =
    case hasTransConstraint @MonadIO @t2 @m of
        Dict -> lift2ComposeT

lift1ComposeTWithUnlift ::
       (MonadTransTunnel t1, MonadTransUnliftAll t2, MonadUnliftIO m)
    => ((forall a. ComposeT t1 t2 m a -> t1 m a) -> t1 m r)
    -> ComposeT t1 t2 m r
lift1ComposeTWithUnlift call =
    MkComposeT $ tunnel $ \tun -> liftWithUnliftAll $ \unlift -> tun $ call $ \(MkComposeT ttma) -> remonad' unlift ttma

lift2ComposeTWithUnlift ::
       forall t1 t2 m r. (MonadTransUnliftAll t1, MonadTransUnliftAll t2, MonadUnliftIO m)
    => ((forall a. ComposeT t1 t2 m a -> t2 m a) -> t2 m r)
    -> ComposeT t1 t2 m r
lift2ComposeTWithUnlift call =
    case hasTransConstraint @MonadUnliftIO @t2 @m of
        Dict -> MkComposeT $ liftWithUnliftAll $ \unlift -> call $ \(MkComposeT ttma) -> unlift ttma

composeUnliftAll ::
       forall c ta tb. (MonadTransUnliftAll tb, MonadTransConstraint c tb)
    => UnliftAll c ta
    -> UnliftAll c tb
    -> UnliftAll c (ComposeT ta tb)
composeUnliftAll ua ub (MkComposeT tatbma) = ub $ withTransConstraintTM @c $ ua tatbma

composeWUnliftAll ::
       forall c ta tb. (MonadTransUnliftAll tb, MonadTransConstraint c tb)
    => WUnliftAll c ta
    -> WUnliftAll c tb
    -> WUnliftAll c (ComposeT ta tb)
composeWUnliftAll (MkWUnliftAll ua) (MkWUnliftAll ub) = MkWUnliftAll $ composeUnliftAll @c ua ub

instance (MonadTrans t1, MonadTransConstraint Monad t2) => MonadTrans (ComposeT t1 t2) where
    lift (ma :: m a) =
        case hasTransConstraint @Monad @t2 @m of
            Dict -> MkComposeT $ lift $ lift ma

instance (MonadTransConstraint Monad t1, MonadTransConstraint Monad t2) => MonadTransConstraint Monad (ComposeT t1 t2) where
    hasTransConstraint ::
           forall m. Monad m
        => Dict (Monad (ComposeT t1 t2 m))
    hasTransConstraint =
        case hasTransConstraint @Monad @t2 @m of
            Dict ->
                case hasTransConstraint @Monad @t1 @(t2 m) of
                    Dict -> Dict

instance (MonadTransConstraint MonadIO t1, MonadTransConstraint Monad t2, MonadTransConstraint MonadIO t2) =>
             MonadTransConstraint MonadIO (ComposeT t1 t2) where
    hasTransConstraint ::
           forall m. MonadIO m
        => Dict (MonadIO (ComposeT t1 t2 m))
    hasTransConstraint =
        case hasTransConstraint @MonadIO @t2 @m of
            Dict ->
                case hasTransConstraint @MonadIO @t1 @(t2 m) of
                    Dict -> Dict

instance (MonadTransConstraint MonadFail t1, MonadTransConstraint Monad t2, MonadTransConstraint MonadFail t2) =>
             MonadTransConstraint MonadFail (ComposeT t1 t2) where
    hasTransConstraint ::
           forall m. MonadFail m
        => Dict (MonadFail (ComposeT t1 t2 m))
    hasTransConstraint =
        case hasTransConstraint @MonadFail @t2 @m of
            Dict ->
                case hasTransConstraint @MonadFail @t1 @(t2 m) of
                    Dict -> Dict

instance (MonadTransConstraint MonadFix t1, MonadTransConstraint Monad t2, MonadTransConstraint MonadFix t2) =>
             MonadTransConstraint MonadFix (ComposeT t1 t2) where
    hasTransConstraint ::
           forall m. MonadFix m
        => Dict (MonadFix (ComposeT t1 t2 m))
    hasTransConstraint =
        case hasTransConstraint @MonadFix @t2 @m of
            Dict ->
                case hasTransConstraint @MonadFix @t1 @(t2 m) of
                    Dict -> Dict

instance (MonadTransConstraint MonadPlus t1, MonadTransConstraint Monad t2, MonadTransConstraint MonadPlus t2) =>
             MonadTransConstraint MonadPlus (ComposeT t1 t2) where
    hasTransConstraint ::
           forall m. MonadPlus m
        => Dict (MonadPlus (ComposeT t1 t2 m))
    hasTransConstraint =
        case hasTransConstraint @MonadPlus @t2 @m of
            Dict ->
                case hasTransConstraint @MonadPlus @t1 @(t2 m) of
                    Dict -> Dict

instance (MonadTransSemiTunnel t1, MonadTransSemiTunnel t2) => MonadTransSemiTunnel (ComposeT t1 t2) where
    semitunnel ::
           forall m1 m2 r. (Monad m1, Monad m2)
        => (forall a. (ComposeT t1 t2 m1 r -> m1 a) -> m2 a)
        -> ComposeT t1 t2 m2 r
    semitunnel call =
        case hasTransConstraint @Monad @t2 @m1 of
            Dict ->
                case hasTransConstraint @Monad @t2 @m2 of
                    Dict ->
                        MkComposeT $
                        semitunnel $ \t1m1rm1a ->
                            semitunnel $ \t2m1am1b -> call $ \(MkComposeT t1t2m1r) -> t2m1am1b $ t1m1rm1a $ t1t2m1r

instance (MonadTransTunnel t1, MonadTransTunnel t2) => MonadTransTunnel (ComposeT t1 t2) where
    tunnel :: forall m2 r. (forall a. (forall m1. ComposeT t1 t2 m1 r -> m1 a) -> m2 a) -> ComposeT t1 t2 m2 r
    tunnel call =
        MkComposeT $
        tunnel $ \t1m1rm1a -> tunnel $ \t2m1am1b -> call $ \(MkComposeT t1t2m1r) -> t2m1am1b $ t1m1rm1a $ t1t2m1r
    transExcept ::
           forall m e a. Monad m
        => ComposeT t1 t2 (ExceptT e m) a
        -> ComposeT t1 t2 m (Either e a)
    transExcept (MkComposeT ma) =
        case hasTransConstraint @Monad @t2 @m of
            Dict -> MkComposeT $ transExcept $ remonad' (\t2ea -> ExceptT $ transExcept t2ea) ma

instance (MonadTransCoerce t1, MonadTransCoerce t2, MonadTransConstraint Monad t2) => MonadTransCoerce (ComposeT t1 t2) where
    transCoerce ::
           forall m1 m2. Coercible m1 m2
        => Dict (Coercible (ComposeT t1 t2 m1) (ComposeT t1 t2 m2))
    transCoerce =
        case transCoerce @t2 @m1 @m2 of
            Dict ->
                case transCoerce @t1 @(t2 m1) @(t2 m2) of
                    Dict -> Dict

instance (MonadTransUnlift t1, MonadTransUnlift t2) => MonadTransUnlift (ComposeT t1 t2) where
    liftWithUnlift ::
           forall m. MonadUnliftIO m
        => MBackFunction m (ComposeT t1 t2 m)
    liftWithUnlift call =
        case hasTransConstraint @MonadUnliftIO @t2 @m of
            Dict ->
                MkComposeT $
                liftWithUnlift $ \unlift1 ->
                    liftWithUnlift $ \unlift2 ->
                        call $ \(MkComposeT t1t2ma) -> unlift2 $ withTransConstraintTM @MonadUnliftIO $ unlift1 t1t2ma
    getDiscardingUnlift ::
           forall m. MonadUnliftIO m
        => ComposeT t1 t2 m (WMFunction (ComposeT t1 t2 m) m)
    getDiscardingUnlift =
        case hasTransConstraint @MonadFail @t2 @m of
            Dict ->
                case hasTransConstraint @MonadIO @t2 @m of
                    Dict ->
                        case hasTransConstraint @MonadFix @t2 @m of
                            Dict ->
                                MkComposeT $
                                withTransConstraintTM @Monad $ do
                                    MkWMFunction unlift1 <- getDiscardingUnlift
                                    MkWMFunction unlift2 <- lift getDiscardingUnlift
                                    return $ MkWMFunction $ \(MkComposeT t1t2ma) -> unlift2 $ unlift1 t1t2ma

instance (MonadTransUnliftAll t1, MonadTransUnliftAll t2) => MonadTransUnliftAll (ComposeT t1 t2) where
    insideOut ::
           forall m r. Monad m
        => (forall b. (forall mm a. Monad mm => ComposeT t1 t2 mm a -> mm (a, b)) -> m (r, b))
        -> ComposeT t1 t2 m r
    insideOut call =
        case hasTransConstraint @Monad @t2 @m of
            Dict ->
                MkComposeT $
                insideOut $ \unlift1 ->
                    insideOut $ \unlift2 ->
                        fmap (\(r, (b, c)) -> ((r, b), c)) $
                        call $ \(MkComposeT ctma) ->
                            fmap (\((a, b), c) -> (a, (b, c))) $ unlift2 $ withTransConstraintTM @Monad $ unlift1 ctma
    liftWithUnliftAll ::
           forall m r. MonadIO m
        => (UnliftAll MonadUnliftIO (ComposeT t1 t2) -> m r)
        -> ComposeT t1 t2 m r
    liftWithUnliftAll call =
        case hasTransConstraint @MonadIO @t2 @m of
            Dict ->
                MkComposeT $
                liftWithUnliftAll $ \unlift1 ->
                    liftWithUnliftAll $ \unlift2 ->
                        call $ \(MkComposeT t1t2ma) -> unlift2 $ withTransConstraintTM @MonadUnliftIO $ unlift1 t1t2ma
    getDiscardingUnliftAll ::
           forall m. Monad m
        => ComposeT t1 t2 m (WUnliftAll MonadUnliftIO (ComposeT t1 t2))
    getDiscardingUnliftAll =
        case hasTransConstraint @Monad @t2 @m of
            Dict ->
                MkComposeT $
                withTransConstraintTM @Monad $ do
                    unlift1 <- getDiscardingUnliftAll
                    unlift2 <- lift getDiscardingUnliftAll
                    return $ composeWUnliftAll unlift1 unlift2

instance (MonadTransAskUnlift t1, MonadTransAskUnlift t2) => MonadTransAskUnlift (ComposeT t1 t2) where
    askUnlift ::
           forall m. Monad m
        => ComposeT t1 t2 m (WUnliftAll Monad (ComposeT t1 t2))
    askUnlift =
        case hasTransConstraint @Monad @t2 @m of
            Dict ->
                MkComposeT $
                withTransConstraintTM @Monad $ do
                    unlift1 <- askUnlift
                    unlift2 <- lift askUnlift
                    return $ composeWUnliftAll unlift1 unlift2
