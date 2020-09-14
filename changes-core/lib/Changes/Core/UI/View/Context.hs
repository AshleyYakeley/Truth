module Changes.Core.UI.View.Context where

import Changes.Core.Import
import Changes.Core.Resource

data ViewContext = MkViewContext
    { vcWithUILock :: forall a. IO a -> IO a
    , vcWithoutUILock :: forall a. IO a -> IO a
    , vcResourceContext :: ResourceContext
    , vcExit :: IO ()
    }
