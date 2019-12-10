module Truth.Core.UI.Specifier.Switch where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data SwitchUISpec sel update where
    MkSwitchUISpec :: UpdateFunction update (WholeUpdate (UISpec sel update)) -> SwitchUISpec sel update

instance Show (SwitchUISpec sel update) where
    show _ = "switch"

instance UIType SwitchUISpec where
    uiWitness = $(iowitness [t|SwitchUISpec|])

switchUISpec :: UpdateFunction update (WholeUpdate (UISpec sel update)) -> UISpec sel update
switchUISpec func = MkUISpec $ MkSwitchUISpec func
