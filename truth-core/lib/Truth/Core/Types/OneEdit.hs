module Truth.Core.Types.OneEdit where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.OneReader

newtype OneEdit (f :: Type -> Type) edit =
    MkOneEdit edit

instance Show edit => Show (OneEdit f edit) where
    show (MkOneEdit e) = "one " ++ show e

instance Floating edit edit => Floating (OneEdit f edit) (OneEdit f edit) where
    floatingUpdate (MkOneEdit e1) (MkOneEdit e2) = MkOneEdit $ floatingUpdate e1 e2

type instance EditReader (OneEdit f edit) =
     OneReader f (EditReader edit)

instance (MonadOne f, ApplicableEdit edit) => ApplicableEdit (OneEdit f edit) where
    applyEdit (MkOneEdit _edita) mr ReadHasOne = mr ReadHasOne
    applyEdit (MkOneEdit edita) mr (ReadOne reader) = getComposeM $ applyEdit edita (oneReadFunctionF mr) reader

instance (MonadOne f, InvertibleEdit edit) => InvertibleEdit (OneEdit f edit) where
    invertEdits editas mr = do
        fme <- getComposeM $ invertEdits (fmap (\(MkOneEdit edita) -> edita) editas) (oneReadFunctionF mr)
        return
            (case getMaybeOne fme of
                 Just edits -> fmap MkOneEdit edits
                 _ -> [])

oneLiftAnUpdateFunction ::
       forall t f edita editb. (MonadTransTunnel t, MonadOne f)
    => AnUpdateFunction t edita editb
    -> AnUpdateFunction t (OneEdit f edita) (OneEdit f editb)
oneLiftAnUpdateFunction (MkAnUpdateFunction g u) = let
    ufGet :: ReadFunctionT t (EditReader (OneEdit f edita)) (EditReader (OneEdit f editb))
    ufGet = liftMaybeReadFunction g
    ufUpdate ::
           forall m. MonadIO m
        => OneEdit f edita
        -> MutableRead m (EditReader (OneEdit f edita))
        -> t m [OneEdit f editb]
    ufUpdate (MkOneEdit ea) mr =
        withTransConstraintTM @Monad $
        fmap (fmap MkOneEdit . fromMaybe [] . getMaybeOne) $ transComposeOne $ u ea $ oneReadFunctionF mr
    in MkAnUpdateFunction {..}

oneLiftUpdateFunction ::
       forall f edita editb. MonadOne f
    => UpdateFunction edita editb
    -> UpdateFunction (OneEdit f edita) (OneEdit f editb)
oneLiftUpdateFunction (MkCloseUnlift unlift ef) = MkCloseUnlift unlift $ oneLiftAnUpdateFunction ef

oneLiftAnEditLens ::
       forall t f edita editb. (MonadOne f, MonadTransTunnel t)
    => AnEditLens t edita editb
    -> AnEditLens t (OneEdit f edita) (OneEdit f editb)
oneLiftAnEditLens (MkAnEditLens ef pe) = let
    elFunction = oneLiftAnUpdateFunction ef
    elPutEdits ::
           forall m. MonadIO m
        => [OneEdit f editb]
        -> MutableRead m (EditReader (OneEdit f edita))
        -> t m (Maybe [OneEdit f edita])
    elPutEdits ebs mr =
        withTransConstraintTM @Monad $
        fmap (fmap (fmap MkOneEdit . fromMaybe []) . getMaybeOne) $
        transComposeOne $ pe (fmap (\(MkOneEdit eb) -> eb) ebs) $ oneReadFunctionF mr
    in MkAnEditLens {..}

oneLiftEditLens ::
       forall f edita editb. MonadOne f
    => EditLens edita editb
    -> EditLens (OneEdit f edita) (OneEdit f editb)
oneLiftEditLens (MkCloseUnlift unlift lens) = MkCloseUnlift unlift $ oneLiftAnEditLens lens
