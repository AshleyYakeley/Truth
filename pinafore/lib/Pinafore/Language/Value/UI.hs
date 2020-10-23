{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Value.UI where

import Changes.Core
import Changes.UI.GTK
import Pinafore.Base
import Shapes

newtype LangUI =
    MkLangUI (CreateView Widget)

data LangWindow = MkLangWindow
    { pwClose :: View ()
    , pwWindow :: UIWindow
    }

pinaforeNewWindow :: WindowSpec -> PinaforeAction LangWindow
pinaforeNewWindow uiw = do
    MkWMFunction exitOnClose <- pinaforeGetExitOnClose
    (pwWindow, close) <- pinaforeEarlyCloser $ createViewPinaforeAction $ exitOnClose $ createWindow uiw
    let pwClose = liftIO close
    return $ MkLangWindow {..}

type LangMenuEntry = MenuEntry
