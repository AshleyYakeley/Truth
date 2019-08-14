module Truth.Core.Object.EditContext
    ( UpdateTiming(..)
    , EditSource
    , noEditSource
    , newEditSource
    , EditContext(..)
    ) where

import Truth.Core.Import

data UpdateTiming
    = SynchronousUpdateTiming
    | AsynchronousUpdateTiming
    deriving (Eq)

instance Show UpdateTiming where
    show SynchronousUpdateTiming = "sync"
    show AsynchronousUpdateTiming = "async"

newtype EditSource =
    MkEditSource (Maybe Unique)
    deriving (Eq)

noEditSource :: EditSource
noEditSource = MkEditSource Nothing

newEditSource :: MonadIO m => m EditSource
newEditSource = do
    u <- liftIO newUnique
    return $ MkEditSource $ Just u

data EditContext = MkEditContext
    { editContextSource :: EditSource
    , editContextTiming :: UpdateTiming
    }
