module Truth.Core.UI.Specifier.Button where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UIButton sel edit where
    MkUIButton :: EditFunction edit (WholeEdit Text) -> IO () -> UIButton sel edit

instance Show (UIButton sel edit) where
    show (MkUIButton _ _) = "button"

instance UIType UIButton where
    uiWitness = $(iowitness [t|UIButton|])

uiButton :: EditFunction edit (WholeEdit Text) -> IO () -> UISpec sel edit
uiButton label action = MkUISpec $ MkUIButton label action
