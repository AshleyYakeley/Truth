module Truth.Core.UI.Specifier.Scrolled where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data ScrolledUISpec sel update where
    MkScrolledUISpec :: UISpec sel update -> ScrolledUISpec sel update

instance Show (ScrolledUISpec sel update) where
    show (MkScrolledUISpec spec) = "scrolled " <> show spec

instance UIType ScrolledUISpec where
    uiWitness = $(iowitness [t|ScrolledUISpec|])

scrolledUISpec :: forall update sel. UISpec sel update -> UISpec sel update
scrolledUISpec spec = MkUISpec $ MkScrolledUISpec spec
