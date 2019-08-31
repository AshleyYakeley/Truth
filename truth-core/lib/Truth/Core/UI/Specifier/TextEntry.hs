module Truth.Core.UI.Specifier.TextEntry where

import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data TextAreaUISpecEntry sel update where
    MkTextAreaUISpecEntry :: TextAreaUISpecEntry sel (WholeUpdate Text)

instance Show (TextAreaUISpecEntry sel update) where
    show MkTextAreaUISpecEntry = "text entry"

instance UIType TextAreaUISpecEntry where
    uiWitness = $(iowitness [t|TextAreaUISpecEntry|])

textAreaUISpecEntry :: forall sel. UISpec sel (WholeUpdate Text)
textAreaUISpecEntry = MkUISpec MkTextAreaUISpecEntry
