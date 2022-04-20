module Control.Monad.Ology.General.Exception.Class where

import qualified Control.Exception as CE
import Control.Monad.Ology.Specific.Result
import Import

class Monad m => MonadException m where
    type Exc m :: Type
    throwExc :: Exc m -> m a
    catchExc :: m a -> (Exc m -> m a) -> m a

instance MonadException Identity where
    type Exc Identity = Void
    throwExc = absurd
    catchExc ma _ = ma

instance MonadException ((->) r) where
    type Exc ((->) r) = Void
    throwExc = absurd
    catchExc ma _ = ma

instance Monoid p => MonadException ((,) p) where
    type Exc ((,) p) = Void
    throwExc = absurd
    catchExc ma _ = ma

instance MonadException Maybe where
    type Exc Maybe = ()
    throwExc () = Nothing
    catchExc Nothing handler = handler ()
    catchExc ma _ = ma

instance MonadException [] where
    type Exc [] = ()
    throwExc _ = []
    catchExc [] handler = handler ()
    catchExc ma _ = ma

instance MonadException (Either e) where
    type Exc (Either e) = e
    throwExc = Left
    catchExc (Right a) _ = Right a
    catchExc (Left e) handler = handler e

instance MonadException (Result e) where
    type Exc (Result e) = e
    throwExc = FailureResult
    catchExc (SuccessResult a) _ = SuccessResult a
    catchExc (FailureResult e) handler = handler e

instance MonadException IO where
    type Exc IO = CE.SomeException
    throwExc = CE.throwIO
    catchExc = CE.catch

catchSomeExc ::
       forall m a. MonadException m
    => m a
    -> (Exc m -> m (Maybe a))
    -> m a
catchSomeExc ma handler = catchExc ma $ \e -> handler e >>= maybe (throwExc e) return

fromResultExc ::
       forall m a. MonadException m
    => Result (Exc m) a
    -> m a
fromResultExc (SuccessResult a) = return a
fromResultExc (FailureResult e) = throwExc e

tryExc ::
       forall m a. MonadException m
    => m a
    -> m (Result (Exc m) a)
tryExc ma = catchExc (fmap SuccessResult ma) $ \e -> return $ FailureResult e

onException ::
       forall m a. MonadException m
    => m a
    -> m ()
    -> m a
onException ma handler = catchExc ma $ \ex -> handler >> throwExc ex

catchPureError :: a -> IO (Maybe CE.SomeException)
catchPureError a = catchExc (CE.evaluate a >> return Nothing) $ \e -> return $ Just e
