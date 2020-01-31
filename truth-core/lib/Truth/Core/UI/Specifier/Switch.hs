module Truth.Core.UI.Specifier.Switch where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data SwitchUISpec sel where
    MkSwitchUISpec :: ReadOnlyOpenSubscriber (WholeUpdate (LUISpec sel)) -> SwitchUISpec sel

instance Show (SwitchUISpec sel) where
    show _ = "switch"

instance UIType SwitchUISpec where
    uiWitness = $(iowitness [t|SwitchUISpec|])

switchUISpec :: ReadOnlyOpenSubscriber (WholeUpdate (LUISpec sel)) -> LUISpec sel
switchUISpec func = mkLUISpec $ MkSwitchUISpec func
