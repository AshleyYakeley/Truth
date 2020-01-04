module Truth.Core.UI.Specifier.Label where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data LabelUISpec sel where
    MkLabelUISpec :: ReadOnlyOpenSubscriber (WholeUpdate Text) -> LabelUISpec sel

instance Show (LabelUISpec sel) where
    show (MkLabelUISpec _) = "label"

instance UIType LabelUISpec where
    uiWitness = $(iowitness [t|LabelUISpec|])

labelUISpec :: ReadOnlyOpenSubscriber (WholeUpdate Text) -> UISpec sel
labelUISpec sub = MkUISpec $ MkLabelUISpec sub
