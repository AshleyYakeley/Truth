module Truth.Core.UI.Specifier.TextEditor where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data TextAreaUISpec sel edit where
    MkTextAreaUISpec :: TextAreaUISpec (EditLens (StringEdit Text) (StringEdit Text)) (StringEdit Text)

instance Show (TextAreaUISpec sel edit) where
    show MkTextAreaUISpec = "text-area"

instance UIType TextAreaUISpec where
    uiWitness = $(iowitness [t|TextAreaUISpec|])

textAreaUISpec :: UISpec (EditLens (StringEdit Text) (StringEdit Text)) (StringEdit Text)
textAreaUISpec = MkUISpec MkTextAreaUISpec
