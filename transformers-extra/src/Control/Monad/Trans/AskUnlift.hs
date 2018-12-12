module Control.Monad.Trans.AskUnlift where

import Control.Monad
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Transform
import Control.Monad.Trans.Unlift
import Data.Constraint
import Prelude

-- | A transformer that has no effects (such as state change or output)
class MonadTransUnlift t => MonadTransAskUnlift t where
    askUnlift ::
           forall m. Monad m
        => t m (Unlift t)

-- | A monad that has no effects over IO (such as state change or output)
class MonadUnliftIO m => MonadAskUnliftIO m where
    askUnliftIO :: m (UnliftIO m)

instance MonadAskUnliftIO IO where
    askUnliftIO = return $ MkTransform id

instance (MonadTransAskUnlift t, MonadAskUnliftIO m, MonadFail (t m), MonadIO (t m), MonadFix (t m)) =>
             MonadAskUnliftIO (t m) where
    askUnliftIO = do
        MkUnlift unlift <- askUnlift
        MkTransform unliftIO <- lift askUnliftIO
        return $ MkTransform $ unliftIO . unlift

instance MonadTransAskUnlift t => MonadTransConstraint MonadAskUnliftIO t where
    hasTransConstraint =
        withTransConstraintDict @MonadFail $ withTransConstraintDict @MonadIO $ withTransConstraintDict @MonadFix $ Dict

instance MonadTransAskUnlift IdentityT where
    askUnlift = return identityUnlift

instance MonadTransAskUnlift (ReaderT s) where
    askUnlift = do
        s <- ask
        return $ MkUnlift $ \mr -> runReaderT mr s
