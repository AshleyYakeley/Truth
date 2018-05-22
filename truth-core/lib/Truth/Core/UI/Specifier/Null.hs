module Truth.Core.UI.Specifier.Null where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data UINull seledit edit where
    MkUINull :: UINull seledit edit

instance Show (UINull seledit edit) where
    show MkUINull = "null"

instance UIType UINull where
    uiWitness = $(iowitness [t|UINull|])

uiNull :: UISpec seledit edit
uiNull = MkUISpec MkUINull
