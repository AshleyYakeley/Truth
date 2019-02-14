module Truth.Core.UI.Specifier.Switch where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data SwitchUISpec sel edit where
    MkSwitchUISpec :: EditFunction edit (WholeEdit (UISpec sel edit)) -> SwitchUISpec sel edit

instance Show (SwitchUISpec sel edit) where
    show _ = "switch"

instance UIType SwitchUISpec where
    uiWitness = $(iowitness [t|SwitchUISpec|])

switchUISpec :: EditFunction edit (WholeEdit (UISpec sel edit)) -> UISpec sel edit
switchUISpec func = MkUISpec $ MkSwitchUISpec func
