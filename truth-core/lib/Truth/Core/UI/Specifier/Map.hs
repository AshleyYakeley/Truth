module Truth.Core.UI.Specifier.Map where

import Truth.Core.Import
import Truth.Core.UI.CreateView
import Truth.Core.UI.Specifier.Specifier

data MapUISpec sel where
    MkMapUISpec :: (forall w. CreateView selb w -> CreateView sela w) -> LUISpec selb -> MapUISpec sela

instance Show (MapUISpec sel) where
    show (MkMapUISpec _ _) = "map"

instance UIType MapUISpec where
    uiWitness = $(iowitness [t|MapUISpec|])

mapViewUISpec :: (forall w. CreateView selb w -> CreateView sela w) -> LUISpec selb -> LUISpec sela
mapViewUISpec mv spec = mkLUISpec $ MkMapUISpec mv spec

shimViewUISpec :: CreateView sel () -> LUISpec sel -> LUISpec sel
shimViewUISpec cvshim = mapViewUISpec $ \cvw -> cvshim >> cvw
