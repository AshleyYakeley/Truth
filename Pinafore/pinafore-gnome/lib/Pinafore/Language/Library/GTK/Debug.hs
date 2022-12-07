module Pinafore.Language.Library.GTK.Debug
    ( gtkDebugStuff
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

gtkDebugStuff :: DocTreeEntry (BindDocTree ())
gtkDebugStuff =
    docTreeEntry "Debug.GTK" "Functions for GTK debugging." $
    namespaceRelative
        "Debug.GTK"
        [ mkValEntry "ignoreUpdateUIExceptions" "Drop exceptions from updates" debugIgnoreUpdateUIExceptions
        , mkValEntry "windowInfo" "Get window contents information" uiWindowDebugDescribe
        ]
