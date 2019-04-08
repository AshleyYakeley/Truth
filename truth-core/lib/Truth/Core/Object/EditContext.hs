module Truth.Core.Object.EditContext
    ( EditSource
    , noEditSource
    , newEditSource
    , EditContext(..)
    ) where

import Truth.Core.Import

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
    , editContextAsync :: Bool
    }
