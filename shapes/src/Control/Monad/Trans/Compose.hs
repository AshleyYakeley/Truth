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

lift2ComposeT :: (MonadTrans t1, Monad (t2 m)) => t2 m a -> ComposeT t1 t2 m a
lift2ComposeT t2ma = MkComposeT $ lift t2ma

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
