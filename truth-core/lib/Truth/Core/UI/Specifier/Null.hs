module Truth.Core.UI.Specifier.Null where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data UINull sel edit where
    MkUINull :: UINull sel edit

instance Show (UINull sel edit) where
    show MkUINull = "null"

instance UIType UINull where
    uiWitness = $(iowitness [t|UINull|])

uiNull :: forall edit sel. UISpec sel edit
uiNull = MkUISpec MkUINull
