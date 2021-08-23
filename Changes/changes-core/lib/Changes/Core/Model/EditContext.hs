module Changes.Core.Model.EditContext
    ( EditSource
    , noEditSource
    , newEditSource
    , EditContext(..)
    , editSourceContext
    , noEditContext
    ) where

import Changes.Core.Import

newtype EditSource =
    MkEditSource (Maybe Unique)
    deriving (Eq)

noEditSource :: EditSource
noEditSource = MkEditSource Nothing

newEditSource :: MonadIO m => m EditSource
newEditSource = do
    u <- liftIO newUnique
    return $ MkEditSource $ Just u

newtype EditContext = MkEditContext
    { editContextSource :: EditSource
    } deriving (Eq)

editSourceContext :: EditSource -> EditContext
editSourceContext editContextSource = MkEditContext {..}

noEditContext :: EditContext
noEditContext = editSourceContext noEditSource
