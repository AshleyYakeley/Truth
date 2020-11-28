module Pinafore.Language.Library.Debug
    ( debugDocModule
    ) where

import Changes.UI.GTK
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Value
import Shapes

debugMessage :: Text -> IO ()
debugMessage _ = return ()

debugWindowInfo :: LangWindow -> IO Text
debugWindowInfo w = uiWindowDebugDescribe $ pwWindow w

debugDocModule :: DocTree BindDoc
debugDocModule =
    MkDocTree
        "Debug"
        "Functions for debugging."
        [ mkValEntry "debugMessage" "Message to debug output. Does nothing except in debug builds." debugMessage
        , mkValEntry "debugWindowInfo" "Get window contents information" debugWindowInfo
        ]
