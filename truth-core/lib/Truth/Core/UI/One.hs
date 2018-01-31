module Truth.Core.UI.One where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier

data UIOne edit
        -- view can create object
      where
    MkUIMaybe
        :: forall edit. (FullEdit edit)
        => Maybe (EditSubject edit)
        -> UISpec edit
        -> UIOne (MaybeEdit edit)
    MkUIOneWhole
        :: forall f edit. (MonadOne f, FullEdit edit)
        => UISpec edit
        -> UIOne (OneWholeEdit f edit)
        --MkUIOne :: forall f edit. (MonadOne f,Edit edit) => UISpec edit -> UIOne (OneEdit f edit);

instance Show (UIOne edit) where
    show (MkUIMaybe _ uispec) = "maybe " ++ show uispec
    show (MkUIOneWhole uispec) = "one+whole " ++ show uispec
        --show (MkUIOne uispec) = "one " ++ show uispec;

instance UIType UIOne where
    uiWitness = $(iowitness [t|UIOne|])

uiMaybe ::
       forall edit. (FullEdit edit)
    => Maybe (EditSubject edit)
    -> UISpec edit
    -> UISpec (MaybeEdit edit)
uiMaybe msubj spec = MkUISpec $ MkUIMaybe msubj spec

uiOneWhole ::
       forall f edit. (MonadOne f, FullEdit edit)
    => UISpec edit
    -> UISpec (OneWholeEdit f edit)
uiOneWhole spec = MkUISpec $ MkUIOneWhole spec