module Truth.Core.UI.Specifier.Scrolled where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data ScrolledUISpec where
    MkScrolledUISpec :: CVUISpec -> ScrolledUISpec

instance Show ScrolledUISpec where
    show (MkScrolledUISpec _) = "scrolled"

instance UIType ScrolledUISpec where
    uiWitness = $(iowitness [t|ScrolledUISpec|])

scrolledUISpec :: CVUISpec -> CVUISpec
scrolledUISpec spec = mkCVUISpec $ MkScrolledUISpec spec
