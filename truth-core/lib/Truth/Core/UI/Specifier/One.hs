module Truth.Core.UI.Specifier.One where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data OneUISpec sel edit where
    -- view can create object
    MaybeUISpec
        :: forall sel edit. (FullEdit edit)
        => Maybe (EditSubject edit)
        -> UISpec sel edit
        -> OneUISpec sel (MaybeEdit edit)
    OneWholeUISpec
        :: forall sel f edit. (MonadOne f, FullEdit edit)
        => UISpec sel edit
        -> OneUISpec sel (OneWholeEdit f edit)
    --MkOneUISpec :: forall f edit. (MonadOne f,ApplicableEdit edit) => UISpec edit -> OneUISpec (OneEdit f edit);

instance Show (OneUISpec sel edit) where
    show (MaybeUISpec _ uispec) = "maybe " ++ show uispec
    show (OneWholeUISpec uispec) = "one+whole " ++ show uispec
    --show (MkOneUISpec uispec) = "one " ++ show uispec;

instance UIType OneUISpec where
    uiWitness = $(iowitness [t|OneUISpec|])

maybeUISpec ::
       forall sel edit. (FullEdit edit)
    => Maybe (EditSubject edit)
    -> UISpec sel edit
    -> UISpec sel (MaybeEdit edit)
maybeUISpec msubj spec = MkUISpec $ MaybeUISpec msubj spec

oneWholeUISpec ::
       forall sel f edit. (MonadOne f, FullEdit edit)
    => UISpec sel edit
    -> UISpec sel (OneWholeEdit f edit)
oneWholeUISpec spec = MkUISpec $ OneWholeUISpec spec
