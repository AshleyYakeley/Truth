module Truth.Core.UI.Specifier.WithAspect where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data WithAspectUISpec sel where
    MkWithAspectUISpec :: (Aspect sel -> UISpec sel) -> WithAspectUISpec sel

instance Show (WithAspectUISpec sel) where
    show (MkWithAspectUISpec _) = "with-aspect"

instance UIType WithAspectUISpec where
    uiWitness = $(iowitness [t|WithAspectUISpec|])

withAspectUISpec :: (Aspect sel -> UISpec sel) -> UISpec sel
withAspectUISpec f = MkUISpec $ MkWithAspectUISpec f
