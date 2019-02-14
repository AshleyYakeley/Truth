module Truth.Core.UI.Specifier.Checkbox where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data CheckboxUISpec sel edit where
    MkCheckboxUISpec :: EditFunction edit (WholeEdit Text) -> EditLens edit (WholeEdit Bool) -> CheckboxUISpec sel edit
    MkMaybeCheckboxUISpec
        :: EditFunction edit (WholeEdit Text) -> EditLens edit (WholeEdit (Maybe Bool)) -> CheckboxUISpec sel edit

instance Show (CheckboxUISpec sel edit) where
    show (MkCheckboxUISpec _ _) = "checkbox"
    show (MkMaybeCheckboxUISpec _ _) = "maybe-checkbox"

instance UIType CheckboxUISpec where
    uiWitness = $(iowitness [t|CheckboxUISpec|])

checkboxUISpec ::
       forall edit sel. EditFunction edit (WholeEdit Text) -> EditLens edit (WholeEdit Bool) -> UISpec sel edit
checkboxUISpec label lens = MkUISpec $ MkCheckboxUISpec label lens

maybeCheckboxUISpec ::
       forall edit sel. EditFunction edit (WholeEdit Text) -> EditLens edit (WholeEdit (Maybe Bool)) -> UISpec sel edit
maybeCheckboxUISpec label lens = MkUISpec $ MkMaybeCheckboxUISpec label lens
