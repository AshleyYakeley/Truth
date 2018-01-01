module Control.Monad.Trans.Compose where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Unlift
import Shapes.Import

newtype ComposeT (t1 :: (* -> *) -> (* -> *)) (t2 :: (* -> *) -> (* -> *)) (m :: * -> *) (a :: *) = MkComposeT
    { unComposeT :: t1 (t2 m) a
    } deriving (Functor, Applicative, Monad, MonadIO)

lift1ComposeT :: (MonadTransTunnel t1, MonadTrans t2, Monad m) => t1 m a -> ComposeT t1 t2 m a
lift1ComposeT t1ma = MkComposeT $ remonad lift t1ma

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
       (MonadTransTunnel t1, MonadTransUnlift t2, MonadUnliftIO m)
    => ((forall a. ComposeT t1 t2 m a -> t1 m a) -> t1 m r)
    -> ComposeT t1 t2 m r
lift1ComposeTWithUnlift call =
    MkComposeT $ tunnel $ \tun -> liftWithUnlift $ \unlift -> tun $ call $ \(MkComposeT ttma) -> remonad unlift ttma

lift2ComposeTWithUnlift ::
       forall t1 t2 m r. (MonadTransUnlift t1, MonadTransUnlift t2, MonadUnliftIO m)
    => ((forall a. ComposeT t1 t2 m a -> t2 m a) -> t2 m r)
    -> ComposeT t1 t2 m r
lift2ComposeTWithUnlift call =
    case hasTransConstraint @MonadIO @t2 @m of
        Dict -> MkComposeT $ liftWithUnlift $ \unlift -> call $ \(MkComposeT ttma) -> unlift ttma

instance (MonadTrans t1, MonadTransConstraint Monad t2) => MonadTrans (ComposeT t1 t2) where
    lift (ma :: m a) =
        case hasTransConstraint @Monad @t2 @m of
            Dict -> MkComposeT $ lift $ lift ma

instance (MonadTransConstraint Monad t1, MonadTransConstraint Monad t2) =>
         MonadTransConstraint Monad (ComposeT t1 t2) where
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
            Dict -> MkComposeT $ transExcept $ remonad (\t2ea -> ExceptT $ transExcept t2ea) ma

instance (MonadTransUnlift t1, MonadTransUnlift t2) => MonadTransUnlift (ComposeT t1 t2) where
    liftWithUnlift ::
           forall m r. MonadUnliftIO m
        => ((forall a. ComposeT t1 t2 m a -> m a) -> m r)
        -> ComposeT t1 t2 m r
    liftWithUnlift call =
        case hasTransConstraint @MonadIO @t2 @m of
            Dict ->
                MkComposeT $
                liftWithUnlift $ \unlift1 ->
                    liftWithUnlift $ \unlift2 -> call $ \(MkComposeT t1t2ma) -> unlift2 $ unlift1 t1t2ma
    impotent ::
           forall m a. Monad m
        => ComposeT t1 t2 m a
        -> ComposeT t1 t2 m a
    impotent (MkComposeT ttma) =
        case hasTransConstraint @Monad @t2 @m of
            Dict -> MkComposeT $ impotent $ remonad impotent ttma
