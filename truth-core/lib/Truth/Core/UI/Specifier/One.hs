module Truth.Core.UI.Specifier.One where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

data UIOne seledit edit where
    -- view can create object
    MkUIMaybe
        :: forall seledit edit. (FullEdit edit)
        => Maybe (EditSubject edit)
        -> UISpec seledit edit
        -> UIOne seledit (MaybeEdit edit)
    MkUIOneWhole
        :: forall seledit f edit. (MonadOne f, FullEdit edit)
        => UISpec seledit edit
        -> UIOne seledit (OneWholeEdit f edit)
    --MkUIOne :: forall f edit. (MonadOne f,ApplicableEdit edit) => UISpec edit -> UIOne (OneEdit f edit);

instance Show (UIOne seledit edit) where
    show (MkUIMaybe _ uispec) = "maybe " ++ show uispec
    show (MkUIOneWhole uispec) = "one+whole " ++ show uispec
    --show (MkUIOne uispec) = "one " ++ show uispec;

instance UIType UIOne where
    uiWitness = $(iowitness [t|UIOne|])

uiMaybe ::
       forall seledit edit. (FullEdit edit)
    => Maybe (EditSubject edit)
    -> UISpec seledit edit
    -> UISpec seledit (MaybeEdit edit)
uiMaybe msubj spec = MkUISpec $ MkUIMaybe msubj spec

uiOneWhole ::
       forall seledit f edit. (MonadOne f, FullEdit edit)
    => UISpec seledit edit
    -> UISpec seledit (OneWholeEdit f edit)
uiOneWhole spec = MkUISpec $ MkUIOneWhole spec
