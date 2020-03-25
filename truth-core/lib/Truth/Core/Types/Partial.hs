module Truth.Core.Types.Partial
    ( ReaderSet
    , PartialUpdate(..)
    , partialFullEditLens
    , convertUpdateEditLens
    , partialEditLens
    , comapUpdateEditLens
    , partialiseEditLens
    , partialiseReadOnlyEditLens
    , liftPartialEditLens
    , partialConvertEditLens
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.ReadOnly

type ReaderSet reader = forall t. reader t -> Bool

data PartialUpdate update
    = KnownPartialUpdate update
    | UnknownPartialUpdate (ReaderSet (UpdateReader update))

type instance UpdateEdit (PartialUpdate update) = UpdateEdit update

instance IsUpdate update => IsUpdate (PartialUpdate update) where
    editUpdate edit = KnownPartialUpdate $ editUpdate edit

partialFullEditLens ::
       forall update. (IsUpdate update, FullEdit (UpdateEdit update))
    => EditLens (PartialUpdate update) update
partialFullEditLens = let
    elGet ::
           forall m t. MonadIO m
        => Readable m (UpdateReader update)
        -> UpdateReader update t
        -> m t
    elGet mr = mr
    elUpdate ::
           forall m. MonadIO m
        => PartialUpdate update
        -> Readable m (UpdateReader update)
        -> m [update]
    elUpdate (KnownPartialUpdate update) _ = return [update]
    elUpdate (UnknownPartialUpdate _) mr = do
        edits <- getReplaceEdits mr
        return $ fmap editUpdate edits
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> Readable m (UpdateReader update)
        -> m (Maybe [UpdateEdit update])
    elPutEdits edits _ = return $ Just edits
    in MkEditLens {..}

convertUpdateEditLens ::
       forall updateA updateB. UpdateEdit updateA ~ UpdateEdit updateB
    => (updateA -> updateB)
    -> EditLens updateA updateB
convertUpdateEditLens ab = let
    elGet ::
           forall m t. MonadIO m
        => Readable m (UpdateReader updateA)
        -> UpdateReader updateB t
        -> m t
    elGet mr = mr
    elUpdate ::
           forall m. MonadIO m
        => updateA
        -> Readable m (UpdateReader updateA)
        -> m [updateB]
    elUpdate update _ = return [ab update]
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit updateB]
        -> Readable m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    elPutEdits edits _ = return $ Just edits
    in MkEditLens {..}

partialEditLens :: forall update. EditLens update (PartialUpdate update)
partialEditLens = convertUpdateEditLens KnownPartialUpdate

comapUpdateEditLens ::
       forall updateA updateB updateC.
       (updateB -> Either updateC updateA)
    -> UpdateEdit updateA ~ UpdateEdit updateB => EditLens updateA updateC -> EditLens updateB updateC
comapUpdateEditLens bca (MkEditLens g u pe) = let
    u' :: forall m. MonadIO m
       => updateB
       -> Readable m (UpdateReader updateB)
       -> m [updateC]
    u' ub mr =
        case bca ub of
            Left uc -> return [uc]
            Right ua -> u ua mr
    in MkEditLens g u' pe

partialiseEditLens ::
       forall updateA updateB.
       (ReaderSet (UpdateReader updateA) -> ReaderSet (UpdateReader updateB))
    -> EditLens updateA (PartialUpdate updateB)
    -> EditLens (PartialUpdate updateA) (PartialUpdate updateB)
partialiseEditLens maprs =
    comapUpdateEditLens $ \case
        UnknownPartialUpdate upd -> Left $ UnknownPartialUpdate $ maprs upd
        KnownPartialUpdate updateA -> Right updateA

partialiseReadOnlyEditLens ::
       forall updateA updateB.
       (ReaderSet (UpdateReader updateA) -> ReaderSet (UpdateReader updateB))
    -> EditLens updateA (ReadOnlyUpdate (PartialUpdate updateB))
    -> EditLens (PartialUpdate updateA) (ReadOnlyUpdate (PartialUpdate updateB))
partialiseReadOnlyEditLens maprs =
    comapUpdateEditLens $ \case
        UnknownPartialUpdate upd -> Left $ MkReadOnlyUpdate $ UnknownPartialUpdate $ maprs upd
        KnownPartialUpdate updateA -> Right updateA

liftPartialEditLens ::
       forall updateA updateB.
       (ReaderSet (UpdateReader updateA) -> ReaderSet (UpdateReader updateB))
    -> EditLens updateA updateB
    -> EditLens (PartialUpdate updateA) (PartialUpdate updateB)
liftPartialEditLens maprs lens = partialiseEditLens maprs $ partialEditLens . lens

partialConvertEditLens ::
       forall updateA updateB.
       ( IsEditUpdate updateA
       , IsUpdate updateB
       , UpdateSubject updateA ~ UpdateSubject updateB
       , FullEdit (UpdateEdit updateA)
       , SubjectMapEdit (UpdateEdit updateB)
       )
    => EditLens updateA (PartialUpdate updateB)
partialConvertEditLens = let
    elGet :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    elGet mr = mSubjectToReadable $ readableToSubject mr
    elUpdate ::
           forall m. MonadIO m
        => updateA
        -> Readable m (UpdateReader updateA)
        -> m [PartialUpdate updateB]
    elUpdate _ _ = return [UnknownPartialUpdate $ \_ -> True]
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit updateB]
        -> Readable m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    elPutEdits editbs mr = do
        oldsubject <- readableToSubject mr
        newsubject <- mapSubjectEdits editbs oldsubject
        editas <- getReplaceEditsFromSubject newsubject
        return $ Just editas
    in MkEditLens {..}
