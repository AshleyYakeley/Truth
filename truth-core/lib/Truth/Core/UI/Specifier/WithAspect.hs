module Truth.Core.UI.Specifier.WithAspect where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data WithAspectUISpec sel edit where
    MkWithAspectUISpec :: (Aspect sel -> UISpec sel edit) -> WithAspectUISpec sel edit

instance Show (WithAspectUISpec sel edit) where
    show (MkWithAspectUISpec _) = "with-aspect"

instance UIType WithAspectUISpec where
    uiWitness = $(iowitness [t|WithAspectUISpec|])

withAspectUISpec :: (Aspect sel -> UISpec sel edit) -> UISpec sel edit
withAspectUISpec f = MkUISpec $ MkWithAspectUISpec f
