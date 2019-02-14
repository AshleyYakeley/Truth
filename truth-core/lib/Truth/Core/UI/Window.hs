module Truth.Core.UI.Window where

import Shapes

data UIWindow = MkUIWindow
    { uiWindowClose :: IO ()
    }

nullUIWindow :: UIWindow
nullUIWindow = MkUIWindow $ return ()
