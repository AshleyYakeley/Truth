module Truth.Core.Object.ObjectEdit
    ( ObjectReader(..)
    , ObjectEdit
    , ObjectUpdate
    , objectEditLens
    , objectLiftEditLens
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Core.Resource
import Truth.Core.Types

data ObjectReader edit t where
    ReadObjectResourceContext :: ObjectReader edit ResourceContext
    ReadObject :: ObjectReader edit (Object edit)

instance Show (ObjectReader edit t) where
    show ReadObjectResourceContext = "object context"
    show ReadObject = "object"

instance AllWitnessConstraint Show (ObjectReader edit) where
    allWitnessConstraint = Dict

instance (c (Object edit), c ResourceContext) => WitnessConstraint c (ObjectReader edit) where
    witnessConstraint ReadObjectResourceContext = Dict
    witnessConstraint ReadObject = Dict

instance SubjectReader (EditReader edit) => SubjectReader (ObjectReader edit) where
    type ReaderSubject (ObjectReader edit) = EditSubject edit
    subjectToRead _ ReadObjectResourceContext = emptyResourceContext
    subjectToRead subj ReadObject = mapObject (fromReadOnlyRejectingEditLens @(EditUpdate edit)) $ constantObject subj

instance FullSubjectReader (EditReader edit) => FullSubjectReader (ObjectReader edit) where
    readableToSubject mr = do
        rc <- mr ReadObjectResourceContext
        obj <- mr ReadObject
        liftIO $ runResource rc obj $ \anobj -> readableToSubject $ objRead anobj

type ObjectEdit edit = ConstEdit (ObjectReader edit)

type ObjectUpdate update = EditUpdate (ObjectEdit (UpdateEdit update))

objectEditLens :: forall update. EditLens (ObjectUpdate update) update
objectEditLens = let
    elGet :: ReadFunction (ObjectReader (UpdateEdit update)) (UpdateReader update)
    elGet mr rt = do
        rc <- mr ReadObjectResourceContext
        obj <- mr ReadObject
        liftIO $ runResource rc obj $ \anobj -> objRead anobj rt
    elUpdate ::
           forall m. MonadIO m
        => ObjectUpdate update
        -> Readable m (ObjectReader (UpdateEdit update))
        -> m [update]
    elUpdate update _ = never update
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> Readable m (EditReader (ObjectEdit (UpdateEdit update)))
        -> m (Maybe [ObjectEdit (UpdateEdit update)])
    elPutEdits edits mr =
        case nonEmpty edits of
            Nothing -> return $ Just []
            Just edits' -> do
                rc <- mr ReadObjectResourceContext
                obj <- mr ReadObject
                liftIO $
                    runResource rc obj $ \anobj -> do
                        maction <- objEdit anobj edits'
                        case maction of
                            Just action -> action noEditSource
                            Nothing -> liftIO $ fail "objectEditLens: failed"
                return $ Just []
    in MkEditLens {..}

objectLiftEditLens ::
       forall updateA updateB. ApplicableEdit (UpdateEdit updateA)
    => EditLens updateA updateB
    -> EditLens (ObjectUpdate updateA) (ObjectUpdate updateB)
objectLiftEditLens lens = let
    elGet :: ReadFunction (ObjectReader (UpdateEdit updateA)) (ObjectReader (UpdateEdit updateB))
    elGet mr ReadObjectResourceContext = mr ReadObjectResourceContext
    elGet mr ReadObject = do
        object <- mr ReadObject
        return $ mapObject lens object
    elUpdate ::
           forall m. MonadIO m
        => ObjectUpdate updateA
        -> Readable m (ObjectReader (UpdateEdit updateA))
        -> m [ObjectUpdate updateB]
    elUpdate update _ = never update
    elPutEdits ::
           forall m. MonadIO m
        => [ObjectEdit (UpdateEdit updateB)]
        -> Readable m (ObjectReader (UpdateEdit updateA))
        -> m (Maybe [ObjectEdit (UpdateEdit updateA)])
    elPutEdits [] _ = return $ Just []
    elPutEdits (edit:_) _ = never edit
    in MkEditLens {..}
