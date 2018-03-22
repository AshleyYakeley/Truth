module Truth.Core.UI.Specifier.Null where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data UINull edit where
    MkUINull :: UINull edit

instance Show (UINull edit) where
    show MkUINull = "null"

instance UIType UINull where
    uiWitness = $(iowitness [t|UINull|])

uiNull :: UISpec edit
uiNull = MkUISpec MkUINull
