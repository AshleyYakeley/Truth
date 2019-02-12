module Truth.Core.UI.Specifier.Label where

import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UILabel sel edit where
    MkUILabel :: UILabel sel (WholeEdit Text)

instance Show (UILabel sel edit) where
    show MkUILabel = "label"

instance UIType UILabel where
    uiWitness = $(iowitness [t|UILabel|])

uiLabel :: UISpec sel (WholeEdit Text)
uiLabel = MkUISpec MkUILabel
