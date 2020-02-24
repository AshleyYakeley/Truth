module Truth.Core.UI.View.Context where

import Truth.Core.Import
import Truth.Core.Resource

data ViewContext = MkViewContext
    { vcRequest :: forall t. IOWitness t -> Maybe t
    , vcWithUILock :: IO () -> IO ()
    , vcResourceContext :: ResourceContext
    }
