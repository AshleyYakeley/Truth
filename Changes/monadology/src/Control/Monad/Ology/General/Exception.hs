module Control.Monad.Ology.General.Exception
    ( module Control.Monad.Ology.General.Exception.Class
    , module Control.Monad.Ology.General.Exception
    , CE.SomeException
    , CE.evaluate
    ) where

import qualified Control.Exception as CE
import Control.Monad.Ology.General.Exception.Class
import Control.Monad.Ology.General.Function
import Control.Monad.Ology.General.Trans.Tunnel
import Import

mask ::
       forall m b. MonadTunnelIO m
    => ((forall a. m a -> m a) -> m b)
    -> m b
mask call = tunnelIO $ \unlift -> CE.mask $ \restore -> unlift $ call $ hoistIO restore

bracket ::
       forall m a b. (MonadException m, MonadTunnelIO m)
    => m a
    -> (a -> m ())
    -> (a -> m b)
    -> m b
bracket before after thing =
    mask $ \restore -> do
        a <- before
        r <- onException (restore (thing a)) (after a)
        _ <- after a
        return r

finally ::
       forall m a. (MonadException m, MonadTunnelIO m)
    => m a
    -> m ()
    -> m a
finally ma handler = bracket (return ()) (const handler) (const ma)

bracket_ ::
       forall m. (MonadException m, MonadTunnelIO m)
    => m ()
    -> m ()
    -> m --> m
bracket_ before after thing = bracket before (const after) (const thing)
