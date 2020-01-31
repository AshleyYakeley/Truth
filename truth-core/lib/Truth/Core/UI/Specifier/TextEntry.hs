module Truth.Core.UI.Specifier.TextEntry where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data TextEntryUISpec sel where
    MkTextEntryUISpec :: OpenSubscriber (WholeUpdate Text) -> TextEntryUISpec sel

instance Show (TextEntryUISpec sel) where
    show (MkTextEntryUISpec _) = "text entry"

instance UIType TextEntryUISpec where
    uiWitness = $(iowitness [t|TextEntryUISpec|])

textEntryUISpec :: forall sel. OpenSubscriber (WholeUpdate Text) -> LUISpec sel
textEntryUISpec sub = mkLUISpec $ MkTextEntryUISpec sub
