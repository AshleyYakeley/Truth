module Truth.Core.UI.Specifier.Null where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data NullUISpec sel edit where
    MkNullUISpec :: NullUISpec sel edit

instance Show (NullUISpec sel edit) where
    show MkNullUISpec = "null"

instance UIType NullUISpec where
    uiWitness = $(iowitness [t|NullUISpec|])

nullUISpec :: forall edit sel. UISpec sel edit
nullUISpec = MkUISpec MkNullUISpec
