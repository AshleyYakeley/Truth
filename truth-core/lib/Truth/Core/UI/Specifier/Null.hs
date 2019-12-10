module Truth.Core.UI.Specifier.Null where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data NullUISpec sel update where
    MkNullUISpec :: NullUISpec sel update

instance Show (NullUISpec sel update) where
    show MkNullUISpec = "null"

instance UIType NullUISpec where
    uiWitness = $(iowitness [t|NullUISpec|])

nullUISpec :: forall update sel. UISpec sel update
nullUISpec = MkUISpec MkNullUISpec
