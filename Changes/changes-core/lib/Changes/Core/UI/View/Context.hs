module Changes.Core.UI.View.Context where

import Changes.Core.Import
import Changes.Core.Resource

data ViewContext = MkViewContext
    { vcWithUILock :: IO --> IO
    , vcWithoutUILock :: IO --> IO
    , vcRunInMain :: LifeCycle --> IO
    , vcResourceContext :: ResourceContext
    , vcExit :: IO ()
    }
