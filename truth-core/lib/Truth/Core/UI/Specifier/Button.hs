module Truth.Core.UI.Specifier.Button where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data ButtonUISpec sel edit where
    MkButtonUISpec :: EditFunction edit (WholeEdit Text) -> IO () -> ButtonUISpec sel edit

instance Show (ButtonUISpec sel edit) where
    show (MkButtonUISpec _ _) = "button"

instance UIType ButtonUISpec where
    uiWitness = $(iowitness [t|ButtonUISpec|])

buttonUISpec :: EditFunction edit (WholeEdit Text) -> IO () -> UISpec sel edit
buttonUISpec label action = MkUISpec $ MkButtonUISpec label action
