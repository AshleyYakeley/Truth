module Truth.Core.Types.OneWhole where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.OneEdit
import Truth.Core.Types.OneReader
import Truth.Core.Types.Sum
import Truth.Core.Types.SumWhole
import Truth.Core.Types.Whole

type OneWholeUpdate (f :: Type -> Type) update = SumWholeUpdate (OneUpdate f update)

type OneWholeEdit (f :: Type -> Type) edit = SumWholeEdit (OneEdit f edit)

type MaybeEdit edit = OneWholeEdit Maybe edit

type MaybeUpdate update = OneWholeUpdate Maybe update

oneWholeLiftUpdateFunction ::
       forall f updateA updateB.
       (MonadOne f, SubjectReader (UpdateReader updateA), FullSubjectReader (UpdateReader updateB))
    => UpdateFunction updateA updateB
    -> UpdateFunction (OneWholeUpdate f updateA) (OneWholeUpdate f updateB)
oneWholeLiftUpdateFunction = sumWholeLiftUpdateFunction . oneLiftUpdateFunction

-- | suitable for Results; trying to put a failure code will be rejected
oneWholeLiftAnEditLens ::
       forall f updateA updateB.
       ( MonadOne f
       , FullSubjectReader (UpdateReader updateA)
       , ApplicableEdit (UpdateEdit updateA)
       , FullEdit (UpdateEdit updateB)
       )
    => EditLens updateA updateB
    -> EditLens (OneWholeUpdate f updateA) (OneWholeUpdate f updateB)
oneWholeLiftAnEditLens alens = sumWholeLiftAnEditLens pushback $ oneLiftAnEditLens alens
  where
    reshuffle :: forall a. f (Maybe a) -> Maybe (f a)
    reshuffle fma =
        case retrieveOne fma of
            SuccessResult (Just a) -> Just $ pure a
            SuccessResult Nothing -> Nothing
            FailureResult (MkLimit fx) -> Just fx
    pushback ::
           forall m. MonadIO m
        => f (UpdateSubject updateB)
        -> MutableRead m (OneReader f (UpdateReader updateA))
        -> m (Maybe (f (UpdateSubject updateA)))
    pushback fb mr =
        case retrieveOne fb of
            FailureResult (MkLimit fx) -> return $ Just fx
            SuccessResult b ->
                fmap reshuffle $
                getComposeM $ do
                    editbs <- getReplaceEditsFromSubject b
                    meditas <- elPutEdits alens editbs $ oneReadFunctionF mr
                    for meditas $ \editas -> mutableReadToSubject $ applyEdits editas $ oneReadFunctionF mr

-- | suitable for Results; trying to put a failure code will be rejected
oneWholeLiftEditLens ::
       forall f updateA updateB.
       ( MonadOne f
       , FullSubjectReader (UpdateReader updateA)
       , ApplicableEdit (UpdateEdit updateA)
       , FullEdit (UpdateEdit updateB)
       )
    => EditLens updateA updateB
    -> EditLens (OneWholeUpdate f updateA) (OneWholeUpdate f updateB)
oneWholeLiftEditLens = oneWholeLiftAnEditLens

mustExistOneEditLens ::
       forall f update. (MonadOne f, IsUpdate update, FullEdit (UpdateEdit update))
    => String
    -> EditLens (OneWholeUpdate f update) update
mustExistOneEditLens err = let
    ufGet :: ReadFunction (OneReader f (UpdateReader update)) (UpdateReader update)
    ufGet mr rt = do
        ft <- mr $ ReadOne rt
        case retrieveOne ft of
            SuccessResult t -> return t
            FailureResult _ -> liftIO $ fail $ err ++ ": not found"
    ufUpdate ::
           forall m. MonadIO m
        => OneWholeUpdate f update
        -> MutableRead m (OneReader f (UpdateReader update))
        -> m [update]
    ufUpdate (SumUpdateLeft (MkWholeReaderUpdate ft)) _ =
        case retrieveOne ft of
            SuccessResult t -> do
                edits <- getReplaceEditsFromSubject t
                return $ fmap editUpdate edits
            FailureResult _ -> liftIO $ fail $ err ++ ": deleted"
    ufUpdate (SumUpdateRight (MkOneUpdate update)) _ = return [update]
    elFunction :: UpdateFunction (OneWholeUpdate f update) update
    elFunction = MkUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> MutableRead m (OneReader f (UpdateReader update))
        -> m (Maybe [OneWholeEdit f (UpdateEdit update)])
    elPutEdits edits _ = return $ Just $ fmap (SumEditRight . MkOneEdit) edits
    in MkEditLens {..}
