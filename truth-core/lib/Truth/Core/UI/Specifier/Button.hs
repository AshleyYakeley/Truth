module Truth.Core.UI.Specifier.Button where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.View

data UIButton seledit edit where
    MkUIButton :: EditFunction edit (WholeEdit Text) -> View seledit edit () -> UIButton seledit edit

instance Show (UIButton seledit edit) where
    show (MkUIButton _ _) = "button"

instance UIType UIButton where
    uiWitness = $(iowitness [t|UIButton|])

uiButton :: EditFunction edit (WholeEdit Text) -> View seledit edit () -> UISpec seledit edit
uiButton label action = MkUISpec $ MkUIButton label action
