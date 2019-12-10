module Truth.Core.UI.Specifier.Label where

import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data LabelUISpec sel update where
    MkLabelUISpec :: LabelUISpec sel (WholeUpdate Text)

instance Show (LabelUISpec sel update) where
    show MkLabelUISpec = "label"

instance UIType LabelUISpec where
    uiWitness = $(iowitness [t|LabelUISpec|])

labelUISpec :: UISpec sel (WholeUpdate Text)
labelUISpec = MkUISpec MkLabelUISpec
