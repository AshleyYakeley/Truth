module Truth.Core.UI.Specifier.Switch where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data SwitchUISpec sel where
    MkSwitchUISpec :: ReadOnlyOpenSubscriber (WholeUpdate (UISpec sel)) -> SwitchUISpec sel

instance Show (SwitchUISpec sel) where
    show _ = "switch"

instance UIType SwitchUISpec where
    uiWitness = $(iowitness [t|SwitchUISpec|])

switchUISpec :: ReadOnlyOpenSubscriber (WholeUpdate (UISpec sel)) -> UISpec sel
switchUISpec func = MkUISpec $ MkSwitchUISpec func
