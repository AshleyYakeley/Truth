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
import Truth.Core.Object.Run
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
        MkRunnableIO (MkWMFunction unlift) (MkAnObject mro _) <- mr ReadObject
        liftIO $ unlift $ mutableReadToSubject mro

type ObjectEdit edit = NoEdit (ObjectReader edit)

type ObjectUpdate update = EditUpdate (ObjectEdit (UpdateEdit update))

objectEditLens :: forall update. EditLens (ObjectUpdate update) update
objectEditLens = let
    ufGet :: ReadFunctionT IdentityT (ObjectReader (UpdateEdit update)) (UpdateReader update)
    ufGet mr rt = do
        (MkRunnableIO (MkWMFunction run) (MkAnObject r _)) <- lift $ mr ReadObject
        liftIO $ run $ r rt
    ufUpdate ::
           forall m. MonadIO m
        => ObjectUpdate update
        -> MutableRead m (ObjectReader (UpdateEdit update))
        -> IdentityT m [update]
    ufUpdate update _ = never update
    elFunction :: AnUpdateFunction IdentityT (ObjectUpdate update) update
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> MutableRead m (EditReader (ObjectEdit (UpdateEdit update)))
        -> IdentityT m (Maybe [ObjectEdit (UpdateEdit update)])
    elPutEdits edits mr = do
        (MkRunnableIO (MkWMFunction run) (MkAnObject _ e)) <- lift $ mr ReadObject
        liftIO $
            run $ do
                maction <- e edits
                case maction of
                    Just action -> action noEditSource
                    Nothing -> liftIO $ fail "objectEditLens: failed"
        return $ Just []
    in MkRunnableT2 wUnIdentityT $ MkAnEditLens {..}

objectLiftEditLens ::
       forall updateA updateB. ApplicableEdit (UpdateEdit updateA)
    => EditLens updateA updateB
    -> EditLens (ObjectUpdate updateA) (ObjectUpdate updateB)
objectLiftEditLens lens = let
    ufGet :: ReadFunctionT IdentityT (ObjectReader (UpdateEdit updateA)) (ObjectReader (UpdateEdit updateB))
    ufGet mr ReadObject = do
        object <- lift $ mr ReadObject
        return $ mapObject lens object
    ufUpdate ::
           forall m. MonadIO m
        => ObjectUpdate updateA
        -> MutableRead m (ObjectReader (UpdateEdit updateA))
        -> IdentityT m [ObjectUpdate updateB]
    ufUpdate edit _ = never edit
    elFunction :: AnUpdateFunction IdentityT (ObjectUpdate updateA) (ObjectUpdate updateB)
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [ObjectEdit (UpdateEdit updateB)]
        -> MutableRead m (ObjectReader (UpdateEdit updateA))
        -> IdentityT m (Maybe [ObjectEdit (UpdateEdit updateA)])
    elPutEdits [] _ = return $ Just []
    elPutEdits (edit:_) _ = never edit
    in MkRunnableT2 wUnIdentityT $ MkAnEditLens {..}
