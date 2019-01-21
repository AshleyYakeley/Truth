module Truth.Core.UI.Specifier.TextEntry where

import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UITextEntry sel edit where
    MkUITextEntry :: UITextEntry sel (WholeEdit Text)

instance Show (UITextEntry sel edit) where
    show MkUITextEntry = "text entry"

instance UIType UITextEntry where
    uiWitness = $(iowitness [t|UITextEntry|])

uiTextEntry :: forall sel. UISpec sel (WholeEdit Text)
uiTextEntry = MkUISpec MkUITextEntry
