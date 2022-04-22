module Control.Monad.Ology.General.Catch where

import qualified Control.Exception as CE
import Control.Monad.Ology.General.Exception
import Control.Monad.Ology.General.Throw
import Control.Monad.Ology.Specific.Result
import Import

class MonadThrow e m => MonadCatch e m where
    catch :: forall a. m a -> (e -> m a) -> m a

try :: forall m e a. MonadCatch e m
    => m a
    -> m (Result e a)
try ma = catch (fmap SuccessResult ma) $ \e -> return $ FailureResult e

handle ::
       forall m e a. MonadCatch e m
    => (e -> m a)
    -> m a
    -> m a
handle handler ma = catch ma handler

instance CE.Exception e => MonadCatch e IO where
    catch ma handler = catchSomeExc ma $ \e -> for (CE.fromException e) handler
