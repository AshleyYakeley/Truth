module Truth.Core.UI.Specifier.TextEntry where

import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UITextEntry edit where
    MkUITextEntry :: UITextEntry (WholeEdit Text)

instance Show (UITextEntry edit) where
    show MkUITextEntry = "text entry"

instance UIType UITextEntry where
    uiWitness = $(iowitness [t|UITextEntry|])

uiTextEntry :: UISpec (WholeEdit Text)
uiTextEntry = MkUISpec MkUITextEntry
