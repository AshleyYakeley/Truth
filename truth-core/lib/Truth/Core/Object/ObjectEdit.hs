module Truth.Core.Object.ObjectEdit
    ( ObjectReader(..)
    , ObjectEdit
    , ObjectUpdate
    , objectChangeLens
    , objectLiftChangeLens
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
    subjectToRead subj ReadObject = mapObject (fromReadOnlyRejectingChangeLens @(EditUpdate edit)) $ constantObject subj

instance FullSubjectReader (EditReader edit) => FullSubjectReader (ObjectReader edit) where
    readableToSubject mr = do
        rc <- mr ReadObjectResourceContext
        obj <- mr ReadObject
        liftIO $ runResource rc obj $ \anobj -> readableToSubject $ objRead anobj

type ObjectEdit edit = ConstEdit (ObjectReader edit)

type ObjectUpdate update = EditUpdate (ObjectEdit (UpdateEdit update))

objectChangeLens :: forall update. ChangeLens (ObjectUpdate update) update
objectChangeLens = let
    clRead :: ReadFunction (ObjectReader (UpdateEdit update)) (UpdateReader update)
    clRead mr rt = do
        rc <- mr ReadObjectResourceContext
        obj <- mr ReadObject
        liftIO $ runResource rc obj $ \anobj -> objRead anobj rt
    clUpdate ::
           forall m. MonadIO m
        => ObjectUpdate update
        -> Readable m (ObjectReader (UpdateEdit update))
        -> m [update]
    clUpdate update _ = never update
    clPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> Readable m (EditReader (ObjectEdit (UpdateEdit update)))
        -> m (Maybe [ObjectEdit (UpdateEdit update)])
    clPutEdits edits mr =
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
                            Nothing -> liftIO $ fail "objectChangeLens: failed"
                return $ Just []
    in MkChangeLens {..}

objectLiftChangeLens ::
       forall updateA updateB. ApplicableEdit (UpdateEdit updateA)
    => ChangeLens updateA updateB
    -> ChangeLens (ObjectUpdate updateA) (ObjectUpdate updateB)
objectLiftChangeLens lens = let
    clRead :: ReadFunction (ObjectReader (UpdateEdit updateA)) (ObjectReader (UpdateEdit updateB))
    clRead mr ReadObjectResourceContext = mr ReadObjectResourceContext
    clRead mr ReadObject = do
        object <- mr ReadObject
        return $ mapObject lens object
    clUpdate ::
           forall m. MonadIO m
        => ObjectUpdate updateA
        -> Readable m (ObjectReader (UpdateEdit updateA))
        -> m [ObjectUpdate updateB]
    clUpdate update _ = never update
    clPutEdits ::
           forall m. MonadIO m
        => [ObjectEdit (UpdateEdit updateB)]
        -> Readable m (ObjectReader (UpdateEdit updateA))
        -> m (Maybe [ObjectEdit (UpdateEdit updateA)])
    clPutEdits [] _ = return $ Just []
    clPutEdits (edit:_) _ = never edit
    in MkChangeLens {..}
