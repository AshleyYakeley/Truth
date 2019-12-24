module Truth.Core.UI.Specifier.TextEditor where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

type TextSelection = LifeCycleIO (EditLens (StringUpdate Text) (StringUpdate Text))

data TextAreaUISpec sel where
    MkTextAreaUISpec :: Subscriber (StringUpdate Text) -> TextAreaUISpec TextSelection

instance Show (TextAreaUISpec sel) where
    show (MkTextAreaUISpec _) = "text-area"

instance UIType TextAreaUISpec where
    uiWitness = $(iowitness [t|TextAreaUISpec|])

textAreaUISpec :: Subscriber (StringUpdate Text) -> UISpec TextSelection
textAreaUISpec sub = MkUISpec $ MkTextAreaUISpec sub
