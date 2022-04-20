module Changes.Core.Types.One.Edit where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.One.Read

newtype OneEdit (f :: Type -> Type) edit =
    MkOneEdit edit

instance Show edit => Show (OneEdit f edit) where
    show (MkOneEdit e) = "one " ++ show e

instance Floating edit edit => Floating (OneEdit f edit) (OneEdit f edit) where
    floatingUpdate (MkOneEdit e1) (MkOneEdit e2) = MkOneEdit $ floatingUpdate e1 e2

type instance EditReader (OneEdit f edit) =
     OneReader f (EditReader edit)

instance (MonadInner f, ApplicableEdit edit) => ApplicableEdit (OneEdit f edit) where
    applyEdit (MkOneEdit _edita) mr ReadHasOne = mr ReadHasOne
    applyEdit (MkOneEdit edita) mr (ReadOne rd) = getComposeInner $ applyEdit edita (oneReadFunctionF mr) rd

instance (MonadInner f, InvertibleEdit edit) => InvertibleEdit (OneEdit f edit) where
    invertEdits editas mr = do
        fme <- getComposeInner $ invertEdits (fmap (\(MkOneEdit edita) -> edita) editas) (oneReadFunctionF mr)
        return
            (case mToMaybe fme of
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

oneLiftChangeLens ::
       forall f updateA updateB. MonadInner f
    => ChangeLens updateA updateB
    -> ChangeLens (OneUpdate f updateA) (OneUpdate f updateB)
oneLiftChangeLens (MkChangeLens g u pe) = let
    clRead :: ReadFunction (OneReader f (UpdateReader updateA)) (OneReader f (UpdateReader updateB))
    clRead = liftOneReadFunction g
    clUpdate ::
           forall m. MonadIO m
        => OneUpdate f updateA
        -> Readable m (OneReader f (UpdateReader updateA))
        -> m [OneUpdate f updateB]
    clUpdate (MkOneUpdate ea) mr =
        fmap (fmap MkOneUpdate . fromMaybe [] . mToMaybe) $ getComposeInner $ u ea $ oneReadFunctionF mr
    clPutEdits ::
           forall m. MonadIO m
        => [OneEdit f (UpdateEdit updateB)]
        -> Readable m (OneReader f (UpdateReader updateA))
        -> m (Maybe [OneEdit f (UpdateEdit updateA)])
    clPutEdits ebs mr =
        fmap (fmap (fmap MkOneEdit . fromMaybe []) . mToMaybe) $
        getComposeInner $ pe (fmap (\(MkOneEdit eb) -> eb) ebs) $ oneReadFunctionF mr
    in MkChangeLens {..}

oneNullChangeLens ::
       forall f updateA updateB. MonadInner f
    => (forall x. f x)
    -> ChangeLens (OneUpdate f updateA) (OneUpdate f updateB)
oneNullChangeLens fu = let
    clRead :: ReadFunction (OneReader f (UpdateReader updateA)) (OneReader f (UpdateReader updateB))
    clRead _ ReadHasOne = return fu
    clRead _ (ReadOne _) = return fu
    clUpdate ::
           forall m. MonadIO m
        => OneUpdate f updateA
        -> Readable m (OneReader f (UpdateReader updateA))
        -> m [OneUpdate f updateB]
    clUpdate _ _ = return []
    clPutEdits ::
           forall m. MonadIO m
        => [OneEdit f (UpdateEdit updateB)]
        -> Readable m (OneReader f (UpdateReader updateA))
        -> m (Maybe [OneEdit f (UpdateEdit updateA)])
    clPutEdits _ _ = return $ Just []
    in MkChangeLens {..}

oneLiftFloatingChangeLens ::
       forall f updateA updateB. MonadInner f
    => FloatingChangeLens updateA updateB
    -> FloatingChangeLens (OneUpdate f updateA) (OneUpdate f updateB)
oneLiftFloatingChangeLens (MkFloatingChangeLens (init :: FloatInit _ r) lens) = let
    fclInit :: FloatInit (OneReader f (UpdateReader updateA)) (f r)
    fclInit = mapFFloatInit oneReadFunctionF init
    fclLens :: f r -> ChangeLens (OneUpdate f updateA) (OneUpdate f updateB)
    fclLens fr =
        case retrieveInner fr of
            SuccessResult r -> oneLiftChangeLens $ lens r
            FailureResult fn -> oneNullChangeLens $ throwExc fn
    in MkFloatingChangeLens {..}
