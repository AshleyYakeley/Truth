module Truth.Core.UI.Specifier.TextEntry where

import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UITextEntry seledit edit where
    MkUITextEntry :: UITextEntry seledit (WholeEdit Text)

instance Show (UITextEntry seledit edit) where
    show MkUITextEntry = "text entry"

instance UIType UITextEntry where
    uiWitness = $(iowitness [t|UITextEntry|])

uiTextEntry :: UISpec seledit (WholeEdit Text)
uiTextEntry = MkUISpec MkUITextEntry
