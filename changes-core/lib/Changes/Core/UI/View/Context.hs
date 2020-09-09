module Truth.Core.UI.View.Context where

import Truth.Core.Import
import Truth.Core.Resource

data ViewContext = MkViewContext
    { vcWithUILock :: forall a. IO a -> IO a
    , vcResourceContext :: ResourceContext
    , vcExit :: IO ()
    }
