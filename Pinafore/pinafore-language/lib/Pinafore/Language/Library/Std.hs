module Pinafore.Language.Library.Std
    ( generalStuff
    ) where

import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Actions
import Pinafore.Language.Library.Std.Base
import Pinafore.Language.Library.Std.Lifecycle
import Pinafore.Language.Library.Std.Model
import Shapes

generalStuff :: BindDocTree context
generalStuff =
    headingBDT "General" "General functionality, in the root namespace." $
    mconcat [baseLibEntries, actionsLibEntries, lifecycleLibEntries, modelLibEntries]
