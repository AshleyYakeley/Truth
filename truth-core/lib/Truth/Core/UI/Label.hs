module Truth.Core.UI.Label where

import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier

data UILabel edit where
    MkUILabel :: UILabel (WholeEdit Text)

instance Show (UILabel edit) where
    show MkUILabel = "label"

instance UIType UILabel where
    uiWitness = $(iowitness [t|UILabel|])

uiLabel :: UISpec (WholeEdit Text)
uiLabel = MkUISpec MkUILabel
