module Truth.Core.Object.ObjectEdit
    ( ObjectReader(..)
    , ObjectEdit
    , objectEditLens
    , objectLiftEditLens
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Core.Types.None

data ObjectReader edit t where
    ReadObject :: ObjectReader edit (Object edit)

instance Show (ObjectReader edit t) where
    show ReadObject = "object"

instance AllWitnessConstraint Show (ObjectReader edit) where
    allWitnessConstraint = Dict

instance c (Object edit) => WitnessConstraint c (ObjectReader edit) where
    witnessConstraint ReadObject = Dict

instance SubjectReader (EditReader edit) => SubjectReader (ObjectReader edit) where
    type ReaderSubject (ObjectReader edit) = EditSubject edit
    subjectToRead subj ReadObject = constantObject subj

instance FullSubjectReader (EditReader edit) => FullSubjectReader (ObjectReader edit) where
    mutableReadToSubject mr = do
        MkCloseUnliftIO (MkTransform unlift) (MkAnObject mro _) <- mr ReadObject
        liftIO $ unlift $ mutableReadToSubject mro

type ObjectEdit edit = NoEdit (ObjectReader edit)

objectEditLens :: forall edit. EditLens (ObjectEdit edit) edit
objectEditLens = let
    efGet :: ReadFunctionT IdentityT (ObjectReader edit) (EditReader edit)
    efGet mr rt = do
        (MkCloseUnliftIO (MkTransform run) (MkAnObject r _)) <- lift $ mr ReadObject
        liftIO $ run $ r rt
    efUpdate ::
           forall m. MonadIO m
        => ObjectEdit edit
        -> MutableRead m (ObjectReader edit)
        -> IdentityT m [edit]
    efUpdate edit _ = never edit
    elFunction :: AnEditFunction IdentityT (ObjectEdit edit) edit
    elFunction = MkAnEditFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [edit]
        -> MutableRead m (EditReader (ObjectEdit edit))
        -> IdentityT m (Maybe [ObjectEdit edit])
    elPutEdits edits mr = do
        (MkCloseUnliftIO (MkTransform run) (MkAnObject _ e)) <- lift $ mr ReadObject
        liftIO $
            run $ do
                maction <- e edits
                case maction of
                    Just action -> action noEditSource
                    Nothing -> liftIO $ fail "objectEditLens: failed"
        return $ Just []
    in MkCloseUnlift identityUnlift $ MkAnEditLens {..}

objectLiftEditLens ::
       forall edita editb. ApplicableEdit edita
    => EditLens edita editb
    -> EditLens (ObjectEdit edita) (ObjectEdit editb)
objectLiftEditLens lens = let
    efGet :: ReadFunctionT IdentityT (ObjectReader edita) (ObjectReader editb)
    efGet mr ReadObject = do
        object <- lift $ mr ReadObject
        return $ mapObject lens object
    efUpdate ::
           forall m. MonadIO m
        => ObjectEdit edita
        -> MutableRead m (ObjectReader edita)
        -> IdentityT m [ObjectEdit editb]
    efUpdate edit _ = never edit
    elFunction :: AnEditFunction IdentityT (ObjectEdit edita) (ObjectEdit editb)
    elFunction = MkAnEditFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [ObjectEdit editb]
        -> MutableRead m (ObjectReader edita)
        -> IdentityT m (Maybe [ObjectEdit edita])
    elPutEdits [] _ = return $ Just []
    elPutEdits (edit:_) _ = never edit
    in MkCloseUnlift identityUnlift $ MkAnEditLens {..}
