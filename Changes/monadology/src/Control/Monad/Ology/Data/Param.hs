module Control.Monad.Ology.Data.Param where

import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.ReaderT
import Import


-- | borrowed from the lens package
type Lens' a b = forall f. Functor f => (b -> f b) -> a -> f a

data Param m a = MkParam
    { askD :: m a
    , localD :: (a -> a) -> m --> m
    }

withD :: Param m a -> a -> m --> m
withD p a = localD p $ \_ -> a

mapParam ::
       forall m a b. Functor m
    => Lens' a b
    -> Param m a
    -> Param m b
mapParam l (MkParam askD localD) = let
    askD' = fmap (\a -> getConst $ l Const a) askD
    localD' :: (b -> b) -> m --> m
    localD' f = localD $ \a -> runIdentity $ l (Identity . f) a
    in MkParam askD' localD'

liftParam :: (MonadTransTunnel t, Monad m) => Param m --> Param (t m)
liftParam (MkParam a l) = MkParam (lift a) $ \aa -> hoist $ l aa

readerParam ::
       forall m r. Monad m
    => Param (ReaderT r m) r
readerParam = MkParam {askD = ask, localD = \aa -> local aa}
