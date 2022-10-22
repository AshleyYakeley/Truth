module Pinafore.Language.Library.GTK.Debug
    ( gtkDebugLibraryModule
    ) where

import Changes.Core
import Changes.World.GNOME.GTK
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

debugIgnoreUpdateUIExceptions :: LangWholeModel '( P, Q) -> LangWholeModel '( P, Q)
debugIgnoreUpdateUIExceptions ref = runIdentity $ langWholeModelMapModel (Identity . ignoreUpdateException) ref

gtkDebugLibraryModule :: LibraryModule ()
gtkDebugLibraryModule =
    MkLibraryModule $
    MkDocTree
        "Debug.GTK"
        "Functions for GTK debugging."
        [ mkValEntry "ignoreUpdateUIExceptions" "Drop exceptions from updates" debugIgnoreUpdateUIExceptions
        , mkValEntry "windowInfo" "Get window contents information" uiWindowDebugDescribe
        ]
