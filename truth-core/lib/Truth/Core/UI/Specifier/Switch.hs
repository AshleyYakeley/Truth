module Truth.Core.UI.Specifier.Switch where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UISwitch sel edit where
    MkUISwitch :: EditFunction edit (WholeEdit (UISpec sel edit)) -> UISwitch sel edit

instance Show (UISwitch sel edit) where
    show _ = "switch"

instance UIType UISwitch where
    uiWitness = $(iowitness [t|UISwitch|])

uiSwitch :: EditFunction edit (WholeEdit (UISpec sel edit)) -> UISpec sel edit
uiSwitch func = MkUISpec $ MkUISwitch func
