module Truth.Core.Types.OneEdit where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.OneReader

newtype OneEdit (f :: * -> *) edit =
    MkOneEdit edit

instance Floating edit edit => Floating (OneEdit f edit) (OneEdit f edit) where
    floatingUpdate (MkOneEdit e1) (MkOneEdit e2) = MkOneEdit $ floatingUpdate e1 e2

instance (MonadOne f, Edit edit) => Edit (OneEdit f edit) where
    type EditReader (OneEdit f edit) = OneReader f (EditReader edit)
    applyEdit (MkOneEdit _edita) mr ReadHasOne = mr ReadHasOne
    applyEdit (MkOneEdit edita) mr (ReadOne reader) = getCompose $ applyEdit edita (oneReadFunctionF mr) reader

instance (MonadOne f, InvertibleEdit edit) => InvertibleEdit (OneEdit f edit) where
    invertEdit (MkOneEdit edita) mr = do
        fme <- getCompose $ invertEdit edita (oneReadFunctionF mr)
        return
            (case getMaybeOne fme of
                 Just edits -> fmap MkOneEdit edits
                 _ -> [])

oneLiftAnEditFunction ::
       forall t f edita editb. (MonadTransTunnel t, MonadOne f)
    => AnEditFunction t edita editb
    -> AnEditFunction t (OneEdit f edita) (OneEdit f editb)
oneLiftAnEditFunction (MkAnEditFunction g u) = let
    efGet :: ReadFunctionT t (EditReader (OneEdit f edita)) (EditReader (OneEdit f editb))
    efGet = liftMaybeReadFunction g
    efUpdate ::
           forall m. MonadIO m
        => OneEdit f edita
        -> MutableRead m (EditReader (OneEdit f edita))
        -> t m [OneEdit f editb]
    efUpdate (MkOneEdit ea) mr =
        withTransConstraintTM @Monad $
        fmap (fmap MkOneEdit . fromMaybe [] . getMaybeOne) $ transComposeOne $ u ea $ oneReadFunctionF mr
    in MkAnEditFunction {..}

oneLiftEditFunction ::
       forall f edita editb. MonadOne f
    => EditFunction edita editb
    -> EditFunction (OneEdit f edita) (OneEdit f editb)
oneLiftEditFunction (MkCloseUnlift unlift ef) = MkCloseUnlift unlift $ oneLiftAnEditFunction ef

oneLiftAnEditLens ::
       forall t f edita editb. (MonadOne f, MonadTransTunnel t)
    => AnEditLens t edita editb
    -> AnEditLens t (OneEdit f edita) (OneEdit f editb)
oneLiftAnEditLens (MkAnEditLens ef pe) = let
    elFunction = oneLiftAnEditFunction ef
    elPutEdit ::
           forall m. MonadIO m
        => OneEdit f editb
        -> MutableRead m (EditReader (OneEdit f edita))
        -> t m (Maybe [OneEdit f edita])
    elPutEdit (MkOneEdit eb) mr =
        withTransConstraintTM @Monad $
        fmap (fmap (fmap MkOneEdit . fromMaybe []) . getMaybeOne) $ transComposeOne $ pe eb $ oneReadFunctionF mr
    in MkAnEditLens {..}

oneLiftEditLens ::
       forall f edita editb. MonadOne f
    => EditLens edita editb
    -> EditLens (OneEdit f edita) (OneEdit f editb)
oneLiftEditLens (MkCloseUnlift unlift lens) = MkCloseUnlift unlift $ oneLiftAnEditLens lens
