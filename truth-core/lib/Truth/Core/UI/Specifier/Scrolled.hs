module Truth.Core.UI.Specifier.Scrolled where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data ScrolledUISpec sel edit where
    MkScrolledUISpec :: UISpec sel edit -> ScrolledUISpec sel edit

instance Show (ScrolledUISpec sel edit) where
    show (MkScrolledUISpec spec) = "scrolled " <> show spec

instance UIType ScrolledUISpec where
    uiWitness = $(iowitness [t|ScrolledUISpec|])

scrolledUISpec :: forall edit sel. UISpec sel edit -> UISpec sel edit
scrolledUISpec spec = MkUISpec $ MkScrolledUISpec spec
