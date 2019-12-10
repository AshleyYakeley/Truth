module Truth.Core.UI.Specifier.One where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data OneUISpec sel update where
    -- view can create object
    MaybeUISpec
        :: forall sel update. (IsUpdate update, FullEdit (UpdateEdit update))
        => Maybe (UpdateSubject update)
        -> UISpec sel update
        -> OneUISpec sel (MaybeUpdate update)
    OneWholeUISpec
        :: forall sel f update. (IsUpdate update, MonadOne f, FullEdit (UpdateEdit update))
        => UISpec sel update
        -> OneUISpec sel (OneWholeUpdate f update)

instance Show (OneUISpec sel update) where
    show (MaybeUISpec _ uispec) = "maybe " ++ show uispec
    show (OneWholeUISpec uispec) = "one+whole " ++ show uispec

instance UIType OneUISpec where
    uiWitness = $(iowitness [t|OneUISpec|])

maybeUISpec ::
       forall sel update. (IsUpdate update, FullEdit (UpdateEdit update))
    => Maybe (UpdateSubject update)
    -> UISpec sel update
    -> UISpec sel (MaybeUpdate update)
maybeUISpec msubj spec = MkUISpec $ MaybeUISpec msubj spec

oneWholeUISpec ::
       forall sel f update. (IsUpdate update, MonadOne f, FullEdit (UpdateEdit update))
    => UISpec sel update
    -> UISpec sel (OneWholeUpdate f update)
oneWholeUISpec spec = MkUISpec $ OneWholeUISpec spec
