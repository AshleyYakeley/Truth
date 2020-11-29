module Pinafore.Language.Library.Debug
    ( debugDocModule
    ) where

import Changes.UI.GTK
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Shapes

debugMessage :: Text -> IO ()
debugMessage t = hPutStrLn stderr $ unpack t

debugDocModule :: DocTree BindDoc
debugDocModule =
    MkDocTree
        "Debug"
        "Functions for debugging."
        [ mkValEntry "debugMessage" "Debug message to std error." debugMessage
        , mkValEntry "debugWindowInfo" "Get window contents information" uiWindowDebugDescribe
        ]
