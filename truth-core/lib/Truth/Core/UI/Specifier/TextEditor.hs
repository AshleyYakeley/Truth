module Truth.Core.UI.Specifier.TextEditor where

import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Reference
import Truth.Core.Types
import Truth.Core.UI.Specifier.Selection
import Truth.Core.UI.Specifier.Specifier

type TextSelection = FloatingChangeLens (StringUpdate Text) (StringUpdate Text)

data TextAreaUISpec where
    MkTextAreaUISpec :: Model (StringUpdate Text) -> SelectNotify TextSelection -> TextAreaUISpec

instance Show TextAreaUISpec where
    show (MkTextAreaUISpec _ _) = "text-area"

instance UIType TextAreaUISpec where
    uiWitness = $(iowitness [t|TextAreaUISpec|])

textAreaUISpec :: Model (StringUpdate Text) -> SelectNotify TextSelection -> CVUISpec
textAreaUISpec sub sel = mkCVUISpec $ MkTextAreaUISpec sub sel
