module Truth.Core.UI.Checkbox where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier

data UICheckbox edit where
    MkUICheckbox :: EditFunction edit (WholeEdit Text) -> EditLens edit (WholeEdit Bool) -> UICheckbox edit
    MkUIMaybeCheckbox :: EditFunction edit (WholeEdit Text) -> EditLens edit (WholeEdit (Maybe Bool)) -> UICheckbox edit

instance Show (UICheckbox edit) where
    show (MkUICheckbox _ _) = "checkbox"
    show (MkUIMaybeCheckbox _ _) = "maybe-checkbox"

instance UIType UICheckbox where
    uiWitness = $(iowitness [t|UICheckbox|])

uiCheckbox :: EditFunction edit (WholeEdit Text) -> EditLens edit (WholeEdit Bool) -> UISpec edit
uiCheckbox label lens = MkUISpec $ MkUICheckbox label lens

uiMaybeCheckbox :: EditFunction edit (WholeEdit Text) -> EditLens edit (WholeEdit (Maybe Bool)) -> UISpec edit
uiMaybeCheckbox label lens = MkUISpec $ MkUIMaybeCheckbox label lens
