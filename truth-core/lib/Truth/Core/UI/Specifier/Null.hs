module Truth.Core.UI.Specifier.Null where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data NullUISpec where
    MkNullUISpec :: NullUISpec

instance Show NullUISpec where
    show MkNullUISpec = "null"

instance UIType NullUISpec where
    uiWitness = $(iowitness [t|NullUISpec|])

nullUISpec :: CVUISpec
nullUISpec = mkCVUISpec MkNullUISpec
