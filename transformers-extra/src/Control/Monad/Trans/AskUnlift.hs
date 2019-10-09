module Control.Monad.Trans.AskUnlift where

import Control.Monad
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Function
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Unlift
import Data.Constraint
import Prelude

-- | A transformer that has no effects (such as state change or output)
class MonadTransUntrans t => MonadTransAskUnlift t where
    askUnlift ::
           forall m. Monad m
        => t m (WUntransFunction t)

-- | A monad that has no effects over IO (such as state change or output)
class MonadUnliftIO m => MonadAskUnliftIO m where
    askUnliftIO :: m (WIOFunction m)

instance MonadAskUnliftIO IO where
    askUnliftIO = return $ MkWMFunction id

instance (MonadTransAskUnlift t, MonadAskUnliftIO m, MonadFail (t m), MonadIO (t m), MonadFix (t m)) =>
             MonadAskUnliftIO (t m) where
    askUnliftIO = do
        MkWUntransFunction unlift <- askUnlift
        MkWMFunction unliftIO <- lift askUnliftIO
        return $ MkWMFunction $ unliftIO . unlift

instance MonadTransAskUnlift t => MonadTransConstraint MonadAskUnliftIO t where
    hasTransConstraint =
        withTransConstraintDict @MonadFail $ withTransConstraintDict @MonadIO $ withTransConstraintDict @MonadFix $ Dict

instance MonadTransAskUnlift IdentityT where
    askUnlift = return wUnIdentityT

instance MonadTransAskUnlift (ReaderT s) where
    askUnlift = do
        s <- ask
        return $ MkWUntransFunction $ \mr -> runReaderT mr s
