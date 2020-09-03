{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Value.UI where

import Pinafore.Base
import Shapes
import Truth.Core
import Truth.UI.GTK

type LangUI = CreateView Widget

data LangWindow = MkLangWindow
    { pwClose :: View ()
    , pwWindow :: UIWindow
    }

pinaforeNewWindow :: WindowSpec -> PinaforeAction LangWindow
pinaforeNewWindow uiw = do
    MkWMFunction exitOnClose <- pinaforeGetExitOnClose
    (pwWindow, close) <- pinaforeLiftLifeCycleIO $ lifeCycleEarlyCloser $ exitOnClose $ createWindow uiw
    let pwClose = liftIO close
    return $ MkLangWindow {..}

type LangMenuEntry = MenuEntry
