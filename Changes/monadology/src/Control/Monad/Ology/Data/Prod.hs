module Control.Monad.Ology.Data.Prod where

import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.WriterT
import Import


data Prod m a = MkProd
    { tellD :: a -> m ()
    , listenD :: forall r. m r -> m (r, a)
    }

listen_D :: Functor m => Prod m a -> m () -> m a
listen_D p mu = fmap snd $ listenD p mu

liftProd :: (MonadTransTunnel t, Monad m) => Prod m --> Prod (t m)
liftProd (MkProd t l) =
    MkProd (\a -> lift $ t a) $ \tmr -> tunnel $ \unlift -> fmap (\(tun, a) -> fmap (\r -> (r, a)) tun) $ l $ unlift tmr

writerProd :: Monad m => Prod (WriterT w m) w
writerProd = MkProd {tellD = tell, listenD = listen}

foldProd ::
       forall f m a. (Applicative f, Foldable f, Applicative m)
    => Prod m a
    -> Prod m (f a)
foldProd (MkProd tellD listenD) = let
    tellD' :: f a -> m ()
    tellD' aa = for_ aa tellD
    listenD' :: forall r. m r -> m (r, f a)
    listenD' mr = fmap (\(r, a) -> (r, pure a)) $ listenD mr
    in MkProd tellD' listenD'
