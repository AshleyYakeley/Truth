module Truth.Core.UI.Specifier.TextEditor where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data TextAreaUISpec sel update where
    MkTextAreaUISpec :: TextAreaUISpec (EditLens (StringUpdate Text) (StringUpdate Text)) (StringUpdate Text)

instance Show (TextAreaUISpec sel update) where
    show MkTextAreaUISpec = "text-area"

instance UIType TextAreaUISpec where
    uiWitness = $(iowitness [t|TextAreaUISpec|])

textAreaUISpec :: UISpec (EditLens (StringUpdate Text) (StringUpdate Text)) (StringUpdate Text)
textAreaUISpec = MkUISpec MkTextAreaUISpec
