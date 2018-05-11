module Truth.Core.UI.Specifier.Checkbox where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UICheckbox seledit edit where
    MkUICheckbox :: EditFunction edit (WholeEdit Text) -> EditLens edit (WholeEdit Bool) -> UICheckbox seledit edit
    MkUIMaybeCheckbox
        :: EditFunction edit (WholeEdit Text) -> EditLens edit (WholeEdit (Maybe Bool)) -> UICheckbox seledit edit

instance Show (UICheckbox seledit edit) where
    show (MkUICheckbox _ _) = "checkbox"
    show (MkUIMaybeCheckbox _ _) = "maybe-checkbox"

instance UIType UICheckbox where
    uiWitness = $(iowitness [t|UICheckbox|])

uiCheckbox :: EditFunction edit (WholeEdit Text) -> EditLens edit (WholeEdit Bool) -> UISpec seledit edit
uiCheckbox label lens = MkUISpec $ MkUICheckbox label lens

uiMaybeCheckbox :: EditFunction edit (WholeEdit Text) -> EditLens edit (WholeEdit (Maybe Bool)) -> UISpec seledit edit
uiMaybeCheckbox label lens = MkUISpec $ MkUIMaybeCheckbox label lens
