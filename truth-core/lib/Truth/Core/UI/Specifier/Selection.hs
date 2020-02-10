module Truth.Core.UI.Specifier.Selection where

import Truth.Core.Import
import Truth.Core.UI.CreateView
import Truth.Core.UI.Specifier.Map
import Truth.Core.UI.Specifier.Specifier

mapSelectionUISpec :: forall sela selb. (sela -> LifeCycleIO selb) -> LUISpec sela -> LUISpec selb
mapSelectionUISpec f = mapViewUISpec $ cvMapSelection f

noSelectionUISpec :: forall sela selb. LUISpec sela -> LUISpec selb
noSelectionUISpec = mapViewUISpec cvNoAspect
