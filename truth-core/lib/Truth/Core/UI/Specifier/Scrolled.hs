module Truth.Core.UI.Specifier.Scrolled where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data ScrolledUISpec sel where
    MkScrolledUISpec :: UISpec sel -> ScrolledUISpec sel

instance Show (ScrolledUISpec sel) where
    show (MkScrolledUISpec spec) = "scrolled " <> show spec

instance UIType ScrolledUISpec where
    uiWitness = $(iowitness [t|ScrolledUISpec|])

scrolledUISpec :: forall sel. UISpec sel -> UISpec sel
scrolledUISpec spec = MkUISpec $ MkScrolledUISpec spec
