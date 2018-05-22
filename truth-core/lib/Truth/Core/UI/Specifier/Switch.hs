module Truth.Core.UI.Specifier.Switch where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UISwitch seledit edit where
    MkUISwitch :: EditFunction edit (WholeEdit (UISpec seledit edit)) -> UISwitch seledit edit

instance Show (UISwitch seledit edit) where
    show _ = "switch"

instance UIType UISwitch where
    uiWitness = $(iowitness [t|UISwitch|])

uiSwitch :: EditFunction edit (WholeEdit (UISpec seledit edit)) -> UISpec seledit edit
uiSwitch func = MkUISpec $ MkUISwitch func
