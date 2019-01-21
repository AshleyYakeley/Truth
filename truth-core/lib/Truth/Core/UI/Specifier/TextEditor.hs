module Truth.Core.UI.Specifier.TextEditor where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UIText sel edit where
    MkUIText :: UIText (EditLens (StringEdit Text) (StringEdit Text)) (StringEdit Text)

instance Show (UIText sel edit) where
    show MkUIText = "text"

instance UIType UIText where
    uiWitness = $(iowitness [t|UIText|])

uiText :: UISpec (EditLens (StringEdit Text) (StringEdit Text)) (StringEdit Text)
uiText = MkUISpec MkUIText
