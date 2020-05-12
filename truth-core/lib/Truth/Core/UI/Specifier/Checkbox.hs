module Truth.Core.UI.Specifier.Checkbox where

import Truth.Core.Import
import Truth.Core.Reference
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data CheckboxUISpec where
    MkCheckboxUISpec :: Model (ROWUpdate Text) -> Model (WholeUpdate Bool) -> CheckboxUISpec
    MkMaybeCheckboxUISpec :: Model (ROWUpdate Text) -> Model (WholeUpdate (Maybe Bool)) -> CheckboxUISpec

instance Show CheckboxUISpec where
    show (MkCheckboxUISpec _ _) = "checkbox"
    show (MkMaybeCheckboxUISpec _ _) = "maybe-checkbox"

instance UIType CheckboxUISpec where
    uiWitness = $(iowitness [t|CheckboxUISpec|])

checkboxUISpec :: Model (ROWUpdate Text) -> Model (WholeUpdate Bool) -> CVUISpec
checkboxUISpec label lens = mkCVUISpec $ MkCheckboxUISpec label lens

maybeCheckboxUISpec :: Model (ROWUpdate Text) -> Model (WholeUpdate (Maybe Bool)) -> CVUISpec
maybeCheckboxUISpec label lens = mkCVUISpec $ MkMaybeCheckboxUISpec label lens
