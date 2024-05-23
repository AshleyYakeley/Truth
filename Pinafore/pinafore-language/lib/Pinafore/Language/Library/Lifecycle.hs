module Pinafore.Language.Library.Lifecycle
    ( lifecycleLibSection
    ) where

import Import
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Var

lifecycleLibSection :: LibraryStuff context
lifecycleLibSection =
    headingBDS
        "Lifecycles"
        "Ways of managing the closing of things that get opened, most notably UI windows."
        [ namespaceBDS
              "Lifecycle"
              [ valBDS
                    "run"
                    "Close everything that gets opened in the given action.\n\n\
                            \Example: `run $ do openResource; sleep (Seconds 1) end`  \n\
                            \This opens some resource, sleeps for one second, and then closes it again." $
                (actionHoistView viewSubLifecycle) @A
              , addNameInRootBDS $
                valBDS
                    "onClose"
                    "Add this action as to be done when closing.\n\n\
                            \Example: `run $ do onClose $ outputLn.Env \"hello\"; sleep (Seconds 1) end`  \n\
                            \This sleeps for one second, and then outputs \"hello\" (when the lifecycle closes)."
                    actionOnClose
              , addNameInRootBDS $
                valBDS
                    "closer"
                    "Get an (idempotent) action that closes what gets opened in the given action.\n\n\
                            \Example: `(cl,r) <- closer openResource`  \n\
                            \This opens a resource `r`, also creating an action `cl`, that will close the resource when first called (subsequent calls do nothing).\n\
                            \This action will also be run at the end of the lifecycle, only if it hasn't already." $
                actionEarlyCloser @A
              ]
        ]
