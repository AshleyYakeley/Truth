module Pinafore.Language.Library.GTK.Debug
    ( uiDebugLibraryModule
    ) where

import Changes.Core
import Changes.UI.GTK
import Pinafore.Language.API
import Pinafore.Language.Library.GTK.Window ()
import Shapes

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

uiDebugLibraryModule :: LibraryModule
uiDebugLibraryModule =
    MkDocTree
        "Debug.GTK"
        "Functions for GTK debugging."
        [ mkValEntry "ignoreUpdateUIExceptions" "Drop exceptions from updates" debugIgnoreUpdateUIExceptions
        , mkValEntry "windowInfo" "Get window contents information" uiWindowDebugDescribe
        ]
