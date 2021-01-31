module Pinafore.Language.Library.Debug
    ( debugLibraryModule
    ) where

import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std ()
import Shapes
import Changes.Debug

debugMessage :: Text -> IO ()
debugMessage t = traceIOM $ unpack t

debugCheckEntity :: Text -> Entity -> IO ()
debugCheckEntity t e = do
    _ <- evaluate $ checkEntity (unpack t) e
    return ()

debugLibraryModule :: LibraryModule
debugLibraryModule =
    MkDocTree
        "Debug"
        "Functions for debugging."
        [ mkValEntry "message" "Debug message to std error." debugMessage
        , mkValEntry "checkEntity" "debugCheckEntity" debugCheckEntity
        ]
