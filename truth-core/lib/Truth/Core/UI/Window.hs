module Truth.Core.UI.Window where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.MenuBar
import Truth.Core.UI.Specifier.Specifier

data WindowSpec = forall sel. MkWindowSpec
    { wsCloseBoxAction :: IO ()
    , wsTitle :: ReadOnlyOpenSubscriber (WholeUpdate Text)
    , wsMenuBar :: Maybe (Aspect sel -> ReadOnlyOpenSubscriber (WholeUpdate MenuBar))
    , wsContent :: LUISpec sel
    }

data UIWindow = MkUIWindow
    { uiWindowHide :: IO ()
    , uiWindowShow :: IO ()
    }

nullUIWindow :: UIWindow
nullUIWindow = let
    uiWindowHide = return ()
    uiWindowShow = return ()
    in MkUIWindow {..}
