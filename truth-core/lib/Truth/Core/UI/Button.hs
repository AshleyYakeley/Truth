module Truth.Core.UI.Button where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier
import Truth.Core.UI.View

data UIButton edit where
    MkUIButton :: EditFunction edit (WholeEdit Text) -> View edit () -> UIButton edit

instance Show (UIButton edit) where
    show (MkUIButton _ _) = "button"

instance UIType UIButton where
    uiWitness = $(iowitness [t|UIButton|])

uiButton :: EditFunction edit (WholeEdit Text) -> View edit () -> UISpec edit
uiButton label action = MkUISpec $ MkUIButton label action
