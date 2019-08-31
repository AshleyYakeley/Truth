module Truth.Core.UI.Specifier.Selection where

import Truth.Core.Import
import Truth.Core.UI.CreateView
import Truth.Core.UI.Specifier.Map
import Truth.Core.UI.Specifier.Specifier

mapSelectionUISpec :: forall sela selb update. (sela -> selb) -> UISpec sela update -> UISpec selb update
mapSelectionUISpec f = mapViewUISpec $ cvMapSelection f

noSelectionUISpec :: forall update sela selb. UISpec sela update -> UISpec selb update
noSelectionUISpec = mapViewUISpec cvNoAspect
