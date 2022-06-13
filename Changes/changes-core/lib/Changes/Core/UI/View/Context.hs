module Changes.Core.UI.View.Context where

import Changes.Core.Import
import Changes.Core.Resource

data ViewContext = MkViewContext
    { vcUnliftLifecycle :: LifeCycle --> IO
    , vcResourceContext :: ResourceContext
    }
