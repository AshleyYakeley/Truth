module Truth.Core.UI.Specifier.TextEditor where

import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

type TextSelection = LifeCycleIO (FloatingEditLens (StringUpdate Text) (StringUpdate Text))

data TextAreaUISpec sel where
    MkTextAreaUISpec :: OpenSubscriber (StringUpdate Text) -> TextAreaUISpec TextSelection

instance Show (TextAreaUISpec sel) where
    show (MkTextAreaUISpec _) = "text-area"

instance UIType TextAreaUISpec where
    uiWitness = $(iowitness [t|TextAreaUISpec|])

textAreaUISpec :: OpenSubscriber (StringUpdate Text) -> LUISpec TextSelection
textAreaUISpec sub = mkLUISpec $ MkTextAreaUISpec sub
