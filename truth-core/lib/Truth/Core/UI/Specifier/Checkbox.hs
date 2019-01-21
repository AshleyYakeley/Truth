module Truth.Core.UI.Specifier.Checkbox where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UICheckbox sel edit where
    MkUICheckbox :: EditFunction edit (WholeEdit Text) -> EditLens edit (WholeEdit Bool) -> UICheckbox sel edit
    MkUIMaybeCheckbox
        :: EditFunction edit (WholeEdit Text) -> EditLens edit (WholeEdit (Maybe Bool)) -> UICheckbox sel edit

instance Show (UICheckbox sel edit) where
    show (MkUICheckbox _ _) = "checkbox"
    show (MkUIMaybeCheckbox _ _) = "maybe-checkbox"

instance UIType UICheckbox where
    uiWitness = $(iowitness [t|UICheckbox|])

uiCheckbox :: forall edit sel. EditFunction edit (WholeEdit Text) -> EditLens edit (WholeEdit Bool) -> UISpec sel edit
uiCheckbox label lens = MkUISpec $ MkUICheckbox label lens

uiMaybeCheckbox ::
       forall edit sel. EditFunction edit (WholeEdit Text) -> EditLens edit (WholeEdit (Maybe Bool)) -> UISpec sel edit
uiMaybeCheckbox label lens = MkUISpec $ MkUIMaybeCheckbox label lens
