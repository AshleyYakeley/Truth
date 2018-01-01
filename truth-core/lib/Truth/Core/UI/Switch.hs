module Truth.Core.UI.Switch where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier

data UISwitch edit where
    MkUISwitch :: EditFunction' edit (WholeEdit (UISpec edit)) -> UISwitch edit

instance Show (UISwitch edit) where
    show _ = "switch"

instance UIType UISwitch where
    uiWitness = $(iowitness [t|UISwitch|])

uiSwitch :: EditFunction' edit (WholeEdit (UISpec edit)) -> UISpec edit
uiSwitch func = MkUISpec $ MkUISwitch func
