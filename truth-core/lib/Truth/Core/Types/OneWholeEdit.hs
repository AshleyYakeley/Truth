module Truth.Core.Types.OneWholeEdit where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.OneEdit
import Truth.Core.Types.OneReader
import Truth.Core.Types.Sum
import Truth.Core.Types.SumWhole
import Truth.Core.Types.Whole

type OneWholeEdit (f :: Type -> Type) edit = SumWholeEdit (OneEdit f edit)

type MaybeEdit edit = OneWholeEdit Maybe edit

oneWholeLiftEditFunction ::
       forall f edita editb. (MonadOne f, SubjectReader (EditReader edita), FullSubjectReader (EditReader editb))
    => EditFunction edita editb
    -> EditFunction (OneWholeEdit f edita) (OneWholeEdit f editb)
oneWholeLiftEditFunction = sumWholeLiftEditFunction . oneLiftEditFunction

-- | suitable for Results; trying to put a failure code will be rejected
oneWholeLiftAnEditLens ::
       forall t f edita editb.
       (MonadTransUnlift t, MonadOne f, FullSubjectReader (EditReader edita), ApplicableEdit edita, FullEdit editb)
    => AnEditLens t edita editb
    -> AnEditLens t (OneWholeEdit f edita) (OneWholeEdit f editb)
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
        => f (EditSubject editb)
        -> MutableRead m (OneReader f (EditReader edita))
        -> t m (Maybe (f (EditSubject edita)))
    pushback fb mr =
        withTransConstraintTM @MonadIO $
        case retrieveOne fb of
            FailureResult (MkLimit fx) -> return $ Just fx
            SuccessResult b ->
                fmap reshuffle $
                transComposeOne $
                withTransConstraintTM @MonadIO $ do
                    editbs <- getReplaceEditsFromSubject b
                    meditas <- elPutEdits alens editbs $ oneReadFunctionF mr
                    for meditas $ \editas -> lift $ mutableReadToSubject $ applyEdits editas $ oneReadFunctionF mr

-- | suitable for Results; trying to put a failure code will be rejected
oneWholeLiftEditLens ::
       forall f edita editb. (MonadOne f, FullSubjectReader (EditReader edita), ApplicableEdit edita, FullEdit editb)
    => EditLens edita editb
    -> EditLens (OneWholeEdit f edita) (OneWholeEdit f editb)
oneWholeLiftEditLens (MkCloseUnlift unlift lens) = MkCloseUnlift unlift $ oneWholeLiftAnEditLens lens

mustExistOneEditLens ::
       forall f edit. (MonadOne f, FullEdit edit)
    => String
    -> EditLens (OneWholeEdit f edit) edit
mustExistOneEditLens err = let
    efGet :: ReadFunctionT IdentityT (OneReader f (EditReader edit)) (EditReader edit)
    efGet mr rt = do
        ft <- lift $ mr $ ReadOne rt
        case retrieveOne ft of
            SuccessResult t -> return t
            FailureResult _ -> liftIO $ fail $ err ++ ": not found"
    efUpdate ::
           forall m. MonadIO m
        => OneWholeEdit f edit
        -> MutableRead m (OneReader f (EditReader edit))
        -> IdentityT m [edit]
    efUpdate (SumEditLeft (MkWholeEdit ft)) _ =
        case retrieveOne ft of
            SuccessResult t -> getReplaceEditsFromSubject t
            FailureResult _ -> liftIO $ fail $ err ++ ": deleted"
    efUpdate (SumEditRight (MkOneEdit edit)) _ = return [edit]
    elFunction :: AnEditFunction IdentityT (OneWholeEdit f edit) edit
    elFunction = MkAnEditFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [edit]
        -> MutableRead m (EditReader (OneWholeEdit f edit))
        -> IdentityT m (Maybe [OneWholeEdit f edit])
    elPutEdits edits _ = return $ Just $ fmap (SumEditRight . MkOneEdit) edits
    in MkCloseUnlift identityUnlift MkAnEditLens {..}
