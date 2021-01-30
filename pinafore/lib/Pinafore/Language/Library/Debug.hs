module Pinafore.Language.Library.Debug
    ( debugLibraryModule
    ) where

import Changes.Core
import Changes.UI.GTK
import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.UI ()
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes

debugMessage :: Text -> IO ()
debugMessage t = hPutStrLn stderr $ unpack t

ignoreGError :: IO () -> IO ()
ignoreGError io = catch io $ \(_ :: GError) -> return ()

ignoreUpdateException :: forall update. WModel update -> WModel update
ignoreUpdateException (MkWModel (MkResource rr amodel)) =
    MkWModel $
    MkResource rr $
    amodel
        { aModelSubscribe =
              \task recv -> aModelSubscribe amodel task $ \rc updates ec -> ignoreGError $ recv rc updates ec
        }

debugIgnoreUpdateUIExceptions :: LangWholeRef '( P, Q) -> LangWholeRef '( P, Q)
debugIgnoreUpdateUIExceptions ref = runIdentity $ langWholeRefMapModel (Identity . ignoreUpdateException) ref

debugCheckEntity :: Text -> Entity -> IO ()
debugCheckEntity t e = do
    _ <- evaluate $ checkEntity (unpack t) e
    return ()

debugLibraryModule :: LibraryModule
debugLibraryModule =
    MkDocTree
        "Debug"
        "Functions for debugging."
        [ mkValEntry "debugMessage" "Debug message to std error." debugMessage
        , mkValEntry "debugIgnoreUpdateUIExceptions" "Drop exceptions from updates" debugIgnoreUpdateUIExceptions
        , mkValEntry "debugWindowInfo" "Get window contents information" uiWindowDebugDescribe
        , mkValEntry "debugCheckEntity" "debugCheckEntity" debugCheckEntity
        ]
