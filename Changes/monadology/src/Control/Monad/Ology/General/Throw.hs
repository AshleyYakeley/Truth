module Control.Monad.Ology.General.Throw
    ( module Control.Monad.Ology.General.Throw
    , CE.Exception(..)
    , CE.ErrorCall
    , pattern CE.ErrorCall
    , CE.IOException
    ) where

import qualified Control.Exception as CE
import Control.Monad.Ology.General.Exception
import Control.Monad.Ology.Specific.Result
import Import

class Monad m => MonadThrow e m where
    throw :: forall a. e -> m a

instance CE.Exception e => MonadThrow e IO where
    throw e = throwExc $ CE.toException e

throwResult ::
       forall m e a. MonadThrow e m
    => Result e a
    -> m a
throwResult (SuccessResult a) = return a
throwResult (FailureResult e) = throw e
