module Truth.Core.Types.One.Edit where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.One.Read

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

type instance UpdateEdit (OneUpdate f update) =
     OneEdit f (UpdateEdit update)

instance IsUpdate update => IsUpdate (OneUpdate f update) where
    editUpdate (MkOneEdit edit) = MkOneUpdate $ editUpdate edit

instance IsEditUpdate update => IsEditUpdate (OneUpdate f update) where
    updateEdit (MkOneUpdate update) = MkOneEdit $ updateEdit update

oneLiftEditLens ::
       forall f updateA updateB. MonadOne f
    => EditLens updateA updateB
    -> EditLens (OneUpdate f updateA) (OneUpdate f updateB)
oneLiftEditLens (MkEditLens g u pe) = let
    elGet :: ReadFunction (OneReader f (UpdateReader updateA)) (OneReader f (UpdateReader updateB))
    elGet = liftOneReadFunction g
    elUpdate ::
           forall m. MonadIO m
        => OneUpdate f updateA
        -> Readable m (OneReader f (UpdateReader updateA))
        -> m [OneUpdate f updateB]
    elUpdate (MkOneUpdate ea) mr =
        fmap (fmap MkOneUpdate . fromMaybe [] . getMaybeOne) $ getComposeM $ u ea $ oneReadFunctionF mr
    elPutEdits ::
           forall m. MonadIO m
        => [OneEdit f (UpdateEdit updateB)]
        -> Readable m (OneReader f (UpdateReader updateA))
        -> m (Maybe [OneEdit f (UpdateEdit updateA)])
    elPutEdits ebs mr =
        fmap (fmap (fmap MkOneEdit . fromMaybe []) . getMaybeOne) $
        getComposeM $ pe (fmap (\(MkOneEdit eb) -> eb) ebs) $ oneReadFunctionF mr
    in MkEditLens {..}

oneNullEditLens ::
       forall f updateA updateB. MonadOne f
    => (forall x. f x)
    -> EditLens (OneUpdate f updateA) (OneUpdate f updateB)
oneNullEditLens fu = let
    elGet :: ReadFunction (OneReader f (UpdateReader updateA)) (OneReader f (UpdateReader updateB))
    elGet _ ReadHasOne = return fu
    elGet _ (ReadOne _) = return fu
    elUpdate ::
           forall m. MonadIO m
        => OneUpdate f updateA
        -> Readable m (OneReader f (UpdateReader updateA))
        -> m [OneUpdate f updateB]
    elUpdate _ _ = return []
    elPutEdits ::
           forall m. MonadIO m
        => [OneEdit f (UpdateEdit updateB)]
        -> Readable m (OneReader f (UpdateReader updateA))
        -> m (Maybe [OneEdit f (UpdateEdit updateA)])
    elPutEdits _ _ = return $ Just []
    in MkEditLens {..}

oneLiftFloatingEditLens ::
       forall f updateA updateB. MonadOne f
    => FloatingEditLens updateA updateB
    -> FloatingEditLens (OneUpdate f updateA) (OneUpdate f updateB)
oneLiftFloatingEditLens (MkFloatingEditLens (init :: FloatInit _ r) lens) = let
    felInit :: FloatInit (OneReader f (UpdateReader updateA)) (f r)
    felInit = mapFFloatInit oneReadFunctionF init
    felLens :: f r -> EditLens (OneUpdate f updateA) (OneUpdate f updateB)
    felLens fr =
        case retrieveOne fr of
            SuccessResult r -> oneLiftEditLens $ lens r
            FailureResult (MkLimit fu) -> oneNullEditLens fu
    in MkFloatingEditLens {..}
