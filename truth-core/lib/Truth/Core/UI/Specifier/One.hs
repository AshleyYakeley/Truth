module Truth.Core.UI.Specifier.One where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data OneUISpec sel where
    -- view can create object
    MaybeUISpec
        :: forall sel update. (IsUpdate update, FullEdit (UpdateEdit update))
        => Maybe (UpdateSubject update)
        -> OpenSubscriber (MaybeUpdate update)
        -> (OpenSubscriber update -> UISpec sel)
        -> OneUISpec sel
    OneWholeUISpec
        :: forall sel f update. (IsUpdate update, MonadOne f, FullEdit (UpdateEdit update))
        => OpenSubscriber (OneWholeUpdate f update)
        -> (OpenSubscriber update -> UISpec sel)
        -> OneUISpec sel

instance Show (OneUISpec sel) where
    show (MaybeUISpec _ _ _) = "maybe"
    show (OneWholeUISpec _ _) = "one+whole"

instance UIType OneUISpec where
    uiWitness = $(iowitness [t|OneUISpec|])

maybeUISpec ::
       forall sel update. (IsUpdate update, FullEdit (UpdateEdit update))
    => Maybe (UpdateSubject update)
    -> OpenSubscriber (MaybeUpdate update)
    -> (OpenSubscriber update -> UISpec sel)
    -> UISpec sel
maybeUISpec msubj sub spec = MkUISpec $ MaybeUISpec msubj sub spec

oneWholeUISpec ::
       forall sel f update. (IsUpdate update, MonadOne f, FullEdit (UpdateEdit update))
    => OpenSubscriber (OneWholeUpdate f update)
    -> (OpenSubscriber update -> UISpec sel)
    -> UISpec sel
oneWholeUISpec sub spec = MkUISpec $ OneWholeUISpec sub spec
