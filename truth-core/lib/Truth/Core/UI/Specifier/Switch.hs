module Truth.Core.UI.Specifier.Switch where

import Truth.Core.Import
import Truth.Core.Reference
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data SwitchUISpec where
    MkSwitchUISpec :: Model (ROWUpdate CVUISpec) -> SwitchUISpec

instance Show SwitchUISpec where
    show _ = "switch"

instance UIType SwitchUISpec where
    uiWitness = $(iowitness [t|SwitchUISpec|])

switchUISpec :: Model (ROWUpdate CVUISpec) -> CVUISpec
switchUISpec func = mkCVUISpec $ MkSwitchUISpec func
