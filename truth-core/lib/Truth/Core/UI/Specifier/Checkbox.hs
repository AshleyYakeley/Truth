module Truth.Core.UI.Specifier.Checkbox where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data CheckboxUISpec sel where
    MkCheckboxUISpec
        :: ReadOnlyOpenSubscriber (WholeUpdate Text) -> OpenSubscriber (WholeUpdate Bool) -> CheckboxUISpec sel
    MkMaybeCheckboxUISpec
        :: ReadOnlyOpenSubscriber (WholeUpdate Text) -> OpenSubscriber (WholeUpdate (Maybe Bool)) -> CheckboxUISpec sel

instance Show (CheckboxUISpec sel) where
    show (MkCheckboxUISpec _ _) = "checkbox"
    show (MkMaybeCheckboxUISpec _ _) = "maybe-checkbox"

instance UIType CheckboxUISpec where
    uiWitness = $(iowitness [t|CheckboxUISpec|])

checkboxUISpec ::
       forall sel. ReadOnlyOpenSubscriber (WholeUpdate Text) -> OpenSubscriber (WholeUpdate Bool) -> LUISpec sel
checkboxUISpec label lens = mkLUISpec $ MkCheckboxUISpec label lens

maybeCheckboxUISpec ::
       forall sel. ReadOnlyOpenSubscriber (WholeUpdate Text) -> OpenSubscriber (WholeUpdate (Maybe Bool)) -> LUISpec sel
maybeCheckboxUISpec label lens = mkLUISpec $ MkMaybeCheckboxUISpec label lens
