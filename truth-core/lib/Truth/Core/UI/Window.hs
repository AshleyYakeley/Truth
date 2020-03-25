module Truth.Core.UI.Window where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.MenuBar
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.View.View

data WindowSpec = MkWindowSpec
    { wsCloseBoxAction :: View ()
    , wsTitle :: Model (ROWUpdate Text)
    , wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
    , wsContent :: CVUISpec
    }

data UIWindow = MkUIWindow
    { uiWindowHide :: View ()
    , uiWindowShow :: View ()
    }

nullUIWindow :: UIWindow
nullUIWindow = let
    uiWindowHide = return ()
    uiWindowShow = return ()
    in MkUIWindow {..}
