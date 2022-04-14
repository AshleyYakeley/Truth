module Control.Monad.Ology.Exception
    ( module Control.Monad.Ology.Exception
    , CE.SomeException
    , CE.Exception(..)
    , CE.evaluate
    , CE.IOException
    , CE.ErrorCall
    , pattern CE.ErrorCall
    ) where

import qualified Control.Exception as CE
import Control.Monad.Ology.Function
import Control.Monad.Ology.Result
import Control.Monad.Ology.Trans.Tunnel
import Control.Monad.Ology.Trans.Unlift
import Import

class Monad m => MonadThrow e m where
    throw :: e -> m a

instance MonadThrow e Maybe where
    throw _ = Nothing

instance MonadThrow e [] where
    throw _ = []

instance MonadThrow e (Result e) where
    throw = FailureResult

instance (MonadTrans t, MonadThrow e m, Monad (t m)) => MonadThrow e (t m) where
    throw e = lift $ throw e

instance {-# OVERLAPPING #-} Monad m => MonadThrow e (ExceptT e m) where
    throw e = ExceptT $ return $ Left e

instance CE.Exception e => MonadThrow e IO where
    throw = CE.throwIO

class MonadThrow e m => MonadCatch e m where
    catch :: m a -> (e -> m a) -> m a

handle ::
       forall e m a. MonadCatch e m
    => (e -> m a)
    -> m a
    -> m a
handle handler ma = catch ma handler

try :: forall e m a. MonadCatch e m
    => m a
    -> m (Either e a)
try ma = catch (fmap Right ma) $ \e -> return $ Left e

throwResult ::
       forall e m a. MonadThrow e m
    => Result e a
    -> m a
throwResult (SuccessResult a) = return a
throwResult (FailureResult e) = throw e

catchResult ::
       forall e m a. MonadCatch e m
    => m a
    -> m (Result e a)
catchResult ma = catch (fmap SuccessResult ma) $ \e -> return $ FailureResult e

instance MonadCatch () Maybe where
    catch (Just a) _ = Just a
    catch Nothing handler = handler ()

instance CE.Exception e => MonadCatch e IO where
    catch = CE.catch

instance (MonadTransTunnel t, MonadCatch e m, Monad (t m)) => MonadCatch e (t m) where
    catch tma handler = tunnel $ \unlift -> catch (unlift tma) $ \e -> unlift $ handler e

instance {-# OVERLAPPING #-} Monad m => MonadCatch e (ExceptT e m) where
    catch (ExceptT mea) handler =
        ExceptT $ do
            ea <- mea
            case ea of
                Left e -> runExceptT $ handler e
                Right a -> return $ Right a

instance MonadCatch e (Result e) where
    catch (SuccessResult a) _ = SuccessResult a
    catch (FailureResult e) handler = handler e

class Monad m => MonadBracket m where
    bracket :: m a -> (a -> m ()) -> (a -> m c) -> m c

bracket_ :: MonadBracket m => m () -> m () -> m --> m
bracket_ before after thing = bracket before (const after) (const thing)

finally :: MonadBracket m => m a -> m () -> m a
finally thing after = bracket_ (return ()) after thing

instance MonadBracket IO where
    bracket = CE.bracket

instance (MonadTransUnlift t, MonadBracket m, MonadUnliftIO m, Monad (t m)) => MonadBracket (t m) where
    bracket before after thing =
        liftWithUnlift $ \unlift -> bracket (unlift before) (\a -> unlift $ after a) (\a -> unlift $ thing a)

catchPureError :: a -> IO (Maybe CE.SomeException)
catchPureError a = catch (CE.evaluate a >> return Nothing) $ \e -> return $ Just e
