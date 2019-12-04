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

newtype OneUpdate (f :: Type -> Type) update =
    MkOneUpdate update

instance IsUpdate update => IsUpdate (OneUpdate f update) where
    type UpdateEdit (OneUpdate f update) = OneEdit f (UpdateEdit update)
    editUpdate (MkOneEdit edit) = MkOneUpdate $ editUpdate edit

instance IsEditUpdate update => IsEditUpdate (OneUpdate f update) where
    updateEdit (MkOneUpdate update) = MkOneEdit $ updateEdit update

oneLiftAnUpdateFunction ::
       forall f updateA updateB. MonadOne f
    => UpdateFunction updateA updateB
    -> UpdateFunction (OneUpdate f updateA) (OneUpdate f updateB)
oneLiftAnUpdateFunction (MkUpdateFunction g u) = let
    ufGet :: ReadFunction (UpdateReader (OneUpdate f updateA)) (UpdateReader (OneUpdate f updateB))
    ufGet = liftMaybeReadFunction g
    ufUpdate ::
           forall m. MonadIO m
        => OneUpdate f updateA
        -> MutableRead m (UpdateReader (OneUpdate f updateA))
        -> m [OneUpdate f updateB]
    ufUpdate (MkOneUpdate ea) mr =
        fmap (fmap MkOneUpdate . fromMaybe [] . getMaybeOne) $ getComposeM $ u ea $ oneReadFunctionF mr
    in MkUpdateFunction {..}

oneLiftUpdateFunction ::
       forall f updateA updateB. MonadOne f
    => UpdateFunction updateA updateB
    -> UpdateFunction (OneUpdate f updateA) (OneUpdate f updateB)
oneLiftUpdateFunction = oneLiftAnUpdateFunction

oneLiftAnEditLens ::
       forall f updateA updateB. MonadOne f
    => EditLens updateA updateB
    -> EditLens (OneUpdate f updateA) (OneUpdate f updateB)
oneLiftAnEditLens (MkEditLens ef pe) = let
    elFunction = oneLiftAnUpdateFunction ef
    elPutEdits ::
           forall m. MonadIO m
        => [OneEdit f (UpdateEdit updateB)]
        -> MutableRead m (EditReader (OneEdit f (UpdateEdit updateA)))
        -> m (Maybe [OneEdit f (UpdateEdit updateA)])
    elPutEdits ebs mr =
        fmap (fmap (fmap MkOneEdit . fromMaybe []) . getMaybeOne) $
        getComposeM $ pe (fmap (\(MkOneEdit eb) -> eb) ebs) $ oneReadFunctionF mr
    in MkEditLens {..}

oneLiftEditLens ::
       forall f updateA updateB. MonadOne f
    => EditLens updateA updateB
    -> EditLens (OneUpdate f updateA) (OneUpdate f updateB)
oneLiftEditLens = oneLiftAnEditLens
