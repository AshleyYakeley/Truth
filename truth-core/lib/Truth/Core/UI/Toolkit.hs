module Truth.Core.UI.Toolkit where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.UI.Window

data UIToolkit = MkUIToolkit
    { uitWithLock :: forall a. IO a -> IO a
    , uitCreateWindow :: forall edit. Subscriber edit -> WindowSpec edit -> IO UIWindow
    , uitCloseAllWindows :: IO ()
    }

nullUIToolkit :: UIToolkit
nullUIToolkit = let
    uitWithLock action = action
    uitCreateWindow _ _ = return nullUIWindow
    uitCloseAllWindows = return ()
    in MkUIToolkit {..}

data TruthContext = MkTruthContext
    { tcArguments :: [String]
    , tcUIToolkit :: UIToolkit
    }

type TruthMain = forall a. (TruthContext -> LifeCycle a) -> IO a
