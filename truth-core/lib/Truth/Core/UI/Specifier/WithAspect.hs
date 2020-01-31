module Truth.Core.UI.Specifier.WithAspect where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data WithAspectUISpec sel where
    MkWithAspectUISpec :: (Aspect sel -> LUISpec sel) -> WithAspectUISpec sel

instance Show (WithAspectUISpec sel) where
    show (MkWithAspectUISpec _) = "with-aspect"

instance UIType WithAspectUISpec where
    uiWitness = $(iowitness [t|WithAspectUISpec|])

withAspectUISpec :: (Aspect sel -> LUISpec sel) -> LUISpec sel
withAspectUISpec f = mkLUISpec $ MkWithAspectUISpec f
