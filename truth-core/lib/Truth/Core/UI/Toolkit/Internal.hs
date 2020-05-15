module Truth.Core.UI.Toolkit.Internal where

import Truth.Core.Import
import Truth.Core.UI.View.CreateView
import Truth.Core.UI.Window

data UIToolkit = MkUIToolkit
    { uitWithLock :: forall a. IO a -> IO a -- ^ run with lock, must not already have it
    , uitCreateWindow :: WindowSpec -> CreateView UIWindow -- ^ must already have lock
    , uitUnliftLifeCycle :: MFunction LifeCycleIO IO -- ^ Closers will be run at the end of the session. (Lock doesn't matter.)
    , uitGetRequest :: forall t. IOWitness t -> Maybe t
    , uitExit :: IO () -- ^ must already have the lock
    }
