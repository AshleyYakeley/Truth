module Truth.Core.Object.ObjectEdit
    ( ObjectReader(..)
    , ObjectEdit
    , ObjectUpdate
    , objectEditLens
    , objectLiftEditLens
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Core.Resource
import Truth.Core.Types

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
        MkRunnable1 trun (MkAnObject mro _) <- mr ReadObject
        runMonoTransStackRunner @IO trun $ \run -> liftIO $ run $ mutableReadToSubject mro

type ObjectEdit edit = NoEdit (ObjectReader edit)

type ObjectUpdate update = EditUpdate (ObjectEdit (UpdateEdit update))

objectEditLens :: forall update. EditLens (ObjectUpdate update) update
objectEditLens = let
    ufGet :: ReadFunction (ObjectReader (UpdateEdit update)) (UpdateReader update)
    ufGet mr rt = do
        (MkRunnable1 trun (MkAnObject r _)) <- mr ReadObject
        runMonoTransStackRunner @IO trun $ \run -> liftIO $ run $ r rt
    ufUpdate ::
           forall m. MonadIO m
        => ObjectUpdate update
        -> MutableRead m (ObjectReader (UpdateEdit update))
        -> m [update]
    ufUpdate update _ = never update
    elFunction :: AnUpdateFunction '[] (ObjectUpdate update) update
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> MutableRead m (EditReader (ObjectEdit (UpdateEdit update)))
        -> m (Maybe [ObjectEdit (UpdateEdit update)])
    elPutEdits edits mr = do
        (MkRunnable1 trun (MkAnObject _ e)) <- mr ReadObject
        runMonoTransStackRunner @IO trun $ \run ->
            liftIO $
            run $ do
                maction <- e edits
                case maction of
                    Just action -> action noEditSource
                    Nothing -> liftIO $ fail "objectEditLens: failed"
        return $ Just []
    in MkRunnable2 cmEmpty $ MkAnEditLens {..}

objectLiftEditLens ::
       forall updateA updateB. ApplicableEdit (UpdateEdit updateA)
    => EditLens updateA updateB
    -> EditLens (ObjectUpdate updateA) (ObjectUpdate updateB)
objectLiftEditLens lens = let
    ufGet :: ReadFunction (ObjectReader (UpdateEdit updateA)) (ObjectReader (UpdateEdit updateB))
    ufGet mr ReadObject = do
        object <- mr ReadObject
        return $ mapObject lens object
    ufUpdate ::
           forall m. MonadIO m
        => ObjectUpdate updateA
        -> MutableRead m (ObjectReader (UpdateEdit updateA))
        -> m [ObjectUpdate updateB]
    ufUpdate edit _ = never edit
    elFunction :: AnUpdateFunction '[] (ObjectUpdate updateA) (ObjectUpdate updateB)
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [ObjectEdit (UpdateEdit updateB)]
        -> MutableRead m (ObjectReader (UpdateEdit updateA))
        -> m (Maybe [ObjectEdit (UpdateEdit updateA)])
    elPutEdits [] _ = return $ Just []
    elPutEdits (edit:_) _ = never edit
    in MkRunnable2 cmEmpty $ MkAnEditLens {..}
