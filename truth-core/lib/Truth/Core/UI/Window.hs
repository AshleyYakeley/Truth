module Truth.Core.UI.Window where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier

data UIWindow actions = forall edit. MkUIWindow
    { uiwTitle :: EditFunction edit (WholeEdit Text)
    , uiwSpec :: UISpec edit
    , uiwSubscriber :: Subscriber edit actions
    }
