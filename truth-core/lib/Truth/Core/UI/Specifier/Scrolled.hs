module Truth.Core.UI.Specifier.Scrolled where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data ScrolledUISpec sel where
    MkScrolledUISpec :: LUISpec sel -> ScrolledUISpec sel

instance Show (ScrolledUISpec sel) where
    show (MkScrolledUISpec _) = "scrolled"

instance UIType ScrolledUISpec where
    uiWitness = $(iowitness [t|ScrolledUISpec|])

scrolledUISpec :: forall sel. LUISpec sel -> LUISpec sel
scrolledUISpec spec = mkLUISpec $ MkScrolledUISpec spec
