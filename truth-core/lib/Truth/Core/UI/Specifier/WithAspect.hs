module Truth.Core.UI.Specifier.WithAspect where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data WithAspectUISpec sel update where
    MkWithAspectUISpec :: (Aspect sel -> UISpec sel update) -> WithAspectUISpec sel update

instance Show (WithAspectUISpec sel update) where
    show (MkWithAspectUISpec _) = "with-aspect"

instance UIType WithAspectUISpec where
    uiWitness = $(iowitness [t|WithAspectUISpec|])

withAspectUISpec :: (Aspect sel -> UISpec sel update) -> UISpec sel update
withAspectUISpec f = MkUISpec $ MkWithAspectUISpec f
