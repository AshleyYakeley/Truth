module Truth.Core.UI.Specifier.WithAspect where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data UIWithAspect sel edit where
    MkUIWithAspect :: (Aspect sel -> UISpec sel edit) -> UIWithAspect sel edit

instance Show (UIWithAspect sel edit) where
    show (MkUIWithAspect _) = "with-aspect"

instance UIType UIWithAspect where
    uiWitness = $(iowitness [t|UIWithAspect|])

uiWithAspect :: (Aspect sel -> UISpec sel edit) -> UISpec sel edit
uiWithAspect f = MkUISpec $ MkUIWithAspect f
