module Truth.Core.UI.Specifier.Label where

import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data LabelUISpec sel edit where
    MkLabelUISpec :: LabelUISpec sel (WholeEdit Text)

instance Show (LabelUISpec sel edit) where
    show MkLabelUISpec = "label"

instance UIType LabelUISpec where
    uiWitness = $(iowitness [t|LabelUISpec|])

labelUISpec :: UISpec sel (WholeEdit Text)
labelUISpec = MkUISpec MkLabelUISpec
