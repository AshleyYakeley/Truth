module Truth.Core.UI.Specifier.TextEntry where

import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data TextAreaUISpecEntry sel edit where
    MkTextAreaUISpecEntry :: TextAreaUISpecEntry sel (WholeEdit Text)

instance Show (TextAreaUISpecEntry sel edit) where
    show MkTextAreaUISpecEntry = "text entry"

instance UIType TextAreaUISpecEntry where
    uiWitness = $(iowitness [t|TextAreaUISpecEntry|])

textAreaUISpecEntry :: forall sel. UISpec sel (WholeEdit Text)
textAreaUISpecEntry = MkUISpec MkTextAreaUISpecEntry
