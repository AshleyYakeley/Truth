module Truth.Core.UI.Specifier.TextEntry where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data TextEntryUISpec where
    MkTextEntryUISpec :: Model (WholeUpdate Text) -> TextEntryUISpec

instance Show TextEntryUISpec where
    show (MkTextEntryUISpec _) = "text entry"

instance UIType TextEntryUISpec where
    uiWitness = $(iowitness [t|TextEntryUISpec|])

textEntryUISpec :: Model (WholeUpdate Text) -> CVUISpec
textEntryUISpec sub = mkCVUISpec $ MkTextEntryUISpec sub
