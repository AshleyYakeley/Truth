module Truth.Core.UI.Specifier.TextEditor where

import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UIText seledit edit where
    MkUIText :: UIText (StringEdit Text) (StringEdit Text)

instance Show (UIText seledit edit) where
    show MkUIText = "text"

instance UIType UIText where
    uiWitness = $(iowitness [t|UIText|])

uiText :: UISpec (StringEdit Text) (StringEdit Text)
uiText = MkUISpec MkUIText
