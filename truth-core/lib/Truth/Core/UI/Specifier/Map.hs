module Truth.Core.UI.Specifier.Map where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier
import Truth.Core.UI.View.CreateView

data MapUISpec where
    MkMapUISpec :: (forall w. CreateView w -> CreateView w) -> CVUISpec -> MapUISpec

instance Show MapUISpec where
    show (MkMapUISpec _ _) = "map"

instance UIType MapUISpec where
    uiWitness = $(iowitness [t|MapUISpec|])

mapViewUISpec :: (forall w. CreateView w -> CreateView w) -> CVUISpec -> CVUISpec
mapViewUISpec mv spec = mkCVUISpec $ MkMapUISpec mv spec
