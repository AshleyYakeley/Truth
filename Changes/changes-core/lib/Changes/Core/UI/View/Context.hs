module Changes.Core.UI.View.Context where

import Changes.Core.Resource

newtype ViewContext = MkViewContext
    { vcResourceContext :: ResourceContext
    }
