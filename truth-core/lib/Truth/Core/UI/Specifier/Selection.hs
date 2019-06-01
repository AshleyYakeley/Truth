module Truth.Core.UI.Specifier.Selection where

import Truth.Core.Import
import Truth.Core.UI.CreateView
import Truth.Core.UI.Specifier.Map
import Truth.Core.UI.Specifier.Specifier

mapSelectionUISpec :: forall sela selb edit. (sela -> selb) -> UISpec sela edit -> UISpec selb edit
mapSelectionUISpec f = mapViewUISpec $ cvMapSelection f

noSelectionUISpec :: forall edit sela selb. UISpec sela edit -> UISpec selb edit
noSelectionUISpec = mapViewUISpec cvNoAspect
