module Truth.Core.UI.Specifier.Label where

import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UILabel seledit edit where
    MkUILabel :: UILabel seledit (WholeEdit Text)

instance Show (UILabel seledit edit) where
    show MkUILabel = "label"

instance UIType UILabel where
    uiWitness = $(iowitness [t|UILabel|])

uiLabel :: UISpec seledit (WholeEdit Text)
uiLabel = MkUISpec MkUILabel
