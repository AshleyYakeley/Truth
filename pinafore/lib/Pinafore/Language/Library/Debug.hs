module Pinafore.Language.Library.Debug
    ( debugDocModule
    ) where

import Changes.Core
import Changes.UI.GTK
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes
import Changes.Debug

debugMessage :: Text -> IO ()
debugMessage t = traceIOM $ unpack t

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

debugDocModule :: DocTree BindDoc
debugDocModule =
    MkDocTree
        "Debug"
        "Functions for debugging."
        [ mkValEntry "debugMessage" "Debug message to std error." debugMessage
        , mkValEntry "debugIgnoreUpdateUIExceptions" "Drop exceptions from updates" debugIgnoreUpdateUIExceptions
        , mkValEntry "debugWindowInfo" "Get window contents information" uiWindowDebugDescribe
        ]
