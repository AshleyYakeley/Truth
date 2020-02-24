module Truth.Core.UI.Specifier.Checkbox where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data CheckboxUISpec where
    MkCheckboxUISpec :: Subscriber (ROWUpdate Text) -> Subscriber (WholeUpdate Bool) -> CheckboxUISpec
    MkMaybeCheckboxUISpec :: Subscriber (ROWUpdate Text) -> Subscriber (WholeUpdate (Maybe Bool)) -> CheckboxUISpec

instance Show CheckboxUISpec where
    show (MkCheckboxUISpec _ _) = "checkbox"
    show (MkMaybeCheckboxUISpec _ _) = "maybe-checkbox"

instance UIType CheckboxUISpec where
    uiWitness = $(iowitness [t|CheckboxUISpec|])

checkboxUISpec :: Subscriber (ROWUpdate Text) -> Subscriber (WholeUpdate Bool) -> CVUISpec
checkboxUISpec label lens = mkCVUISpec $ MkCheckboxUISpec label lens

maybeCheckboxUISpec :: Subscriber (ROWUpdate Text) -> Subscriber (WholeUpdate (Maybe Bool)) -> CVUISpec
maybeCheckboxUISpec label lens = mkCVUISpec $ MkMaybeCheckboxUISpec label lens
