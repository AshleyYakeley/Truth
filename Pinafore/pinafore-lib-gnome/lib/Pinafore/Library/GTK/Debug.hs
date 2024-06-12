module Pinafore.Library.GTK.Debug
    ( gtkDebugStuff
    ) where

import Changes.Core
import Changes.World.GNOME.GTK
import Pinafore.API
import Pinafore.Library.GTK.Context
import Pinafore.Library.GTK.Window ()
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

gtkLock :: LangContext -> Action A -> Action A
gtkLock lc = actionHoistView $ \va -> runGView (lcGTKContext lc) $ gvRunLocked $ gvRunUnlocked $ gvLiftView va

gtkDebugStuff :: LibraryStuff ()
gtkDebugStuff =
    headingBDS "GTK.Debug" "Functions for GTK debugging." $
    pure $
    namespaceBDS
        "GTK.Debug"
        [ valBDS "ignoreUpdateUIExceptions" "Drop exceptions from updates" debugIgnoreUpdateUIExceptions
        , valBDS "windowInfo" "Get window contents information" uiWindowDebugDescribe
        , valBDS "lock" "Lock GTK" gtkLock
        ]
