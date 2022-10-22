module Pinafore.Language.Library.Std.Lifecycle
    ( lifecycleLibEntries
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Var
import Shapes

lifecycleLibEntries :: [DocTreeEntry (BindDoc context)]
lifecycleLibEntries =
    [ docTreeEntry
          "Lifecycles"
          "Ways of managing the closing of things that get opened, most notably UI windows."
          [ mkValEntry
                "lifecycle"
                "Close everything that gets opened in the given action.\n\n\
                \Example: `lifecycle $ do openResource; sleep (Seconds 1) end`  \n\
                \This opens some resource, sleeps for one second, and then closes it again." $
            (actionHoistView viewSubLifecycle) @A
          , mkValEntry
                "onClose"
                "Add this action as to be done when closing.\n\n\
                \Example: `lifecycle $ do onClose $ Env.outputLn \"hello\"; sleep (Seconds 1) end`  \n\
                \This sleeps for one second, and then outputs \"hello\" (when the lifecycle closes)."
                actionOnClose
          , mkValEntry
                "closer"
                "Get an (idempotent) action that closes what gets opened in the given action.\n\n\
                \Example: `(cl,r) <- closer openResource`  \n\
                \This opens a resource `r`, also creating an action `cl`, that will close the resource when first called (subsequent calls do nothing).\n\
                \This action will also be run at the end of the lifecycle, only if it hasn't already." $
            actionEarlyCloser @A
          ]
    ]
