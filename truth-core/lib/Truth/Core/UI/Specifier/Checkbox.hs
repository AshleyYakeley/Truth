module Truth.Core.UI.Specifier.Checkbox where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data CheckboxUISpec sel where
    MkCheckboxUISpec :: ReadOnlySubscriber (WholeUpdate Text) -> Subscriber (WholeUpdate Bool) -> CheckboxUISpec sel
    MkMaybeCheckboxUISpec
        :: ReadOnlySubscriber (WholeUpdate Text) -> Subscriber (WholeUpdate (Maybe Bool)) -> CheckboxUISpec sel

instance Show (CheckboxUISpec sel) where
    show (MkCheckboxUISpec _ _) = "checkbox"
    show (MkMaybeCheckboxUISpec _ _) = "maybe-checkbox"

instance UIType CheckboxUISpec where
    uiWitness = $(iowitness [t|CheckboxUISpec|])

checkboxUISpec :: forall sel. ReadOnlySubscriber (WholeUpdate Text) -> Subscriber (WholeUpdate Bool) -> UISpec sel
checkboxUISpec label lens = MkUISpec $ MkCheckboxUISpec label lens

maybeCheckboxUISpec ::
       forall sel. ReadOnlySubscriber (WholeUpdate Text) -> Subscriber (WholeUpdate (Maybe Bool)) -> UISpec sel
maybeCheckboxUISpec label lens = MkUISpec $ MkMaybeCheckboxUISpec label lens
