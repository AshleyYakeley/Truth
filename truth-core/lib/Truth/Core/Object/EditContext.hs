module Truth.Core.Object.EditContext
    ( UpdateTiming(..)
    , EditSource
    , noEditSource
    , newEditSource
    , EditContext(..)
    , editSourceContext
    , noEditContext
    , timingEditContext
    ) where

import Truth.Core.Import

data UpdateTiming
    = SynchronousUpdateTiming
    | AsynchronousUpdateTiming
    deriving (Eq)

instance Semigroup UpdateTiming where
    SynchronousUpdateTiming <> ut = ut
    AsynchronousUpdateTiming <> _ = AsynchronousUpdateTiming

instance Monoid UpdateTiming where
    mempty = SynchronousUpdateTiming

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
    } deriving (Eq)

editSourceContext :: EditSource -> EditContext
editSourceContext editContextSource = let
    editContextTiming = mempty
    in MkEditContext {..}

noEditContext :: EditContext
noEditContext = editSourceContext noEditSource

timingEditContext :: UpdateTiming -> EditContext -> EditContext
timingEditContext ut (MkEditContext esrc eut) = MkEditContext esrc $ ut <> eut
