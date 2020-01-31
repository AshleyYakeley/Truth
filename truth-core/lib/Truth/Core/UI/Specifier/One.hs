module Truth.Core.UI.Specifier.One where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data OneUISpec sel where
    -- view can create object
    OneWholeUISpec
        :: forall sel f update. (IsUpdate update, MonadOne f, FullEdit (UpdateEdit update))
        => OpenSubscriber (FullResultOneUpdate f update)
        -> (f (OpenSubscriber update) -> LifeCycleIO (UISpec sel))
        -> OneUISpec sel

instance Show (OneUISpec sel) where
    show (OneWholeUISpec _ _) = "one+whole"

instance UIType OneUISpec where
    uiWitness = $(iowitness [t|OneUISpec|])

oneWholeUISpec ::
       forall sel f update. (IsUpdate update, MonadOne f, FullEdit (UpdateEdit update))
    => OpenSubscriber (FullResultOneUpdate f update)
    -> (f (OpenSubscriber update) -> LUISpec sel)
    -> LUISpec sel
oneWholeUISpec sub spec = mkLUISpec $ OneWholeUISpec sub spec
