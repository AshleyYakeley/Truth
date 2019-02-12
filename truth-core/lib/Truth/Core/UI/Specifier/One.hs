module Truth.Core.UI.Specifier.One where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UIOne sel edit where
    -- view can create object
    MkUIMaybe
        :: forall sel edit. (FullEdit edit)
        => Maybe (EditSubject edit)
        -> UISpec sel edit
        -> UIOne sel (MaybeEdit edit)
    MkUIOneWhole
        :: forall sel f edit. (MonadOne f, FullEdit edit)
        => UISpec sel edit
        -> UIOne sel (OneWholeEdit f edit)
    --MkUIOne :: forall f edit. (MonadOne f,ApplicableEdit edit) => UISpec edit -> UIOne (OneEdit f edit);

instance Show (UIOne sel edit) where
    show (MkUIMaybe _ uispec) = "maybe " ++ show uispec
    show (MkUIOneWhole uispec) = "one+whole " ++ show uispec
    --show (MkUIOne uispec) = "one " ++ show uispec;

instance UIType UIOne where
    uiWitness = $(iowitness [t|UIOne|])

uiMaybe ::
       forall sel edit. (FullEdit edit)
    => Maybe (EditSubject edit)
    -> UISpec sel edit
    -> UISpec sel (MaybeEdit edit)
uiMaybe msubj spec = MkUISpec $ MkUIMaybe msubj spec

uiOneWhole ::
       forall sel f edit. (MonadOne f, FullEdit edit)
    => UISpec sel edit
    -> UISpec sel (OneWholeEdit f edit)
uiOneWhole spec = MkUISpec $ MkUIOneWhole spec
