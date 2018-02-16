module Truth.Core.UI.Checkbox where

import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier

data UICheckbox edit where
    MkUICheckbox :: Text -> UICheckbox (WholeEdit Bool)

instance Show (UICheckbox edit) where
    show _ = "checkbox"

instance UIType UICheckbox where
    uiWitness = $(iowitness [t|UICheckbox|])

uiCheckbox :: Text -> UISpec (WholeEdit Bool)
uiCheckbox text = MkUISpec $ MkUICheckbox text
