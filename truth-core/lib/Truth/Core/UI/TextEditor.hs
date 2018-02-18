module Truth.Core.UI.TextEditor where

import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier

data UIText edit where
    MkUIText :: UIText (StringEdit Text)

instance Show (UIText edit) where
    show MkUIText = "text"

instance UIType UIText where
    uiWitness = $(iowitness [t|UIText|])

uiText :: UISpec (StringEdit Text)
uiText = MkUISpec MkUIText
