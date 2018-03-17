module Truth.Core.UI.Checkbox where

import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier

data UICheckbox edit where
    MkUICheckbox :: Text -> UICheckbox (WholeEdit Bool)
    MkUIMaybeCheckbox :: Text -> UICheckbox (WholeEdit (Maybe Bool))

instance Show (UICheckbox edit) where
    show (MkUICheckbox _) = "checkbox"
    show (MkUIMaybeCheckbox _) = "maybe-checkbox"

instance UIType UICheckbox where
    uiWitness = $(iowitness [t|UICheckbox|])

uiCheckbox :: Text -> UISpec (WholeEdit Bool)
uiCheckbox text = MkUISpec $ MkUICheckbox text

uiMaybeCheckbox :: Text -> UISpec (WholeEdit (Maybe Bool))
uiMaybeCheckbox text = MkUISpec $ MkUIMaybeCheckbox text
