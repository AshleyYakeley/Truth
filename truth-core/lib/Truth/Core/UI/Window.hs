module Truth.Core.UI.Window where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.UI.Specifier

data UIWindow actions = forall edit. MkUIWindow
    { uiwTitle :: String
    , uiwSpec :: UISpec edit
    , uiwSubscriber :: Subscriber edit actions
    }
