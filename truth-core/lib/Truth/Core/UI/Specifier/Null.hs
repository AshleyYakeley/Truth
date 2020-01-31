module Truth.Core.UI.Specifier.Null where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data NullUISpec sel where
    MkNullUISpec :: NullUISpec sel

instance Show (NullUISpec sel) where
    show MkNullUISpec = "null"

instance UIType NullUISpec where
    uiWitness = $(iowitness [t|NullUISpec|])

nullUISpec :: forall sel. LUISpec sel
nullUISpec = mkLUISpec MkNullUISpec
