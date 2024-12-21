module Changes.Core.Types.Partial
    ( ReaderSet
    , PartialUpdate(..)
    , partialFullChangeLens
    , convertUpdateChangeLens
    , partialChangeLens
    , comapUpdateChangeLens
    , partialiseChangeLens
    , partialiseReadOnlyChangeLens
    , liftPartialChangeLens
    , partialConvertChangeLens
    ) where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.ReadOnly

type ReaderSet (reader :: Type -> Type) = forall (t :: Type). reader t -> Bool

data PartialUpdate update
    = KnownPartialUpdate update
    | UnknownPartialUpdate (ReaderSet (UpdateReader update))

type instance UpdateEdit (PartialUpdate update) = UpdateEdit update

instance IsUpdate update => IsUpdate (PartialUpdate update) where
    editUpdate edit = KnownPartialUpdate $ editUpdate edit

partialFullChangeLens ::
       forall update. (IsUpdate update, FullEdit (UpdateEdit update))
    => ChangeLens (PartialUpdate update) update
partialFullChangeLens = let
    clRead :: ReadFunction (UpdateReader update) (UpdateReader update)
    clRead mr = mr
    clUpdate ::
           forall m. MonadIO m
        => PartialUpdate update
        -> Readable m (UpdateReader update)
        -> m [update]
    clUpdate (KnownPartialUpdate update) _ = return [update]
    clUpdate (UnknownPartialUpdate _) mr = do
        edits <- getReplaceEdits mr
        return $ fmap editUpdate edits
    clPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> Readable m (UpdateReader update)
        -> m (Maybe [UpdateEdit update])
    clPutEdits edits _ = return $ Just edits
    in MkChangeLens {..}

convertUpdateChangeLens ::
       forall updateA updateB. UpdateEdit updateA ~ UpdateEdit updateB
    => (updateA -> updateB)
    -> ChangeLens updateA updateB
convertUpdateChangeLens ab = let
    clRead :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    clRead mr = mr
    clUpdate ::
           forall m. MonadIO m
        => updateA
        -> Readable m (UpdateReader updateA)
        -> m [updateB]
    clUpdate update _ = return [ab update]
    clPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit updateB]
        -> Readable m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    clPutEdits edits _ = return $ Just edits
    in MkChangeLens {..}

partialChangeLens :: forall update. ChangeLens update (PartialUpdate update)
partialChangeLens = convertUpdateChangeLens KnownPartialUpdate

comapUpdateChangeLens ::
       forall updateA updateB updateC.
       (updateB -> Either updateC updateA)
    -> UpdateEdit updateA ~ UpdateEdit updateB => ChangeLens updateA updateC -> ChangeLens updateB updateC
comapUpdateChangeLens bca (MkChangeLens g u pe) = let
    u' :: forall m. MonadIO m
       => updateB
       -> Readable m (UpdateReader updateB)
       -> m [updateC]
    u' ub mr =
        case bca ub of
            Left uc -> return [uc]
            Right ua -> u ua mr
    in MkChangeLens g u' pe

partialiseChangeLens ::
       forall updateA updateB.
       (ReaderSet (UpdateReader updateA) -> ReaderSet (UpdateReader updateB))
    -> ChangeLens updateA (PartialUpdate updateB)
    -> ChangeLens (PartialUpdate updateA) (PartialUpdate updateB)
partialiseChangeLens maprs =
    comapUpdateChangeLens $ \case
        UnknownPartialUpdate upd -> Left $ UnknownPartialUpdate $ maprs upd
        KnownPartialUpdate updateA -> Right updateA

partialiseReadOnlyChangeLens ::
       forall updateA updateB.
       (ReaderSet (UpdateReader updateA) -> ReaderSet (UpdateReader updateB))
    -> ChangeLens updateA (ReadOnlyUpdate (PartialUpdate updateB))
    -> ChangeLens (PartialUpdate updateA) (ReadOnlyUpdate (PartialUpdate updateB))
partialiseReadOnlyChangeLens maprs =
    comapUpdateChangeLens $ \case
        UnknownPartialUpdate upd -> Left $ MkReadOnlyUpdate $ UnknownPartialUpdate $ maprs upd
        KnownPartialUpdate updateA -> Right updateA

liftPartialChangeLens ::
       forall updateA updateB.
       (ReaderSet (UpdateReader updateA) -> ReaderSet (UpdateReader updateB))
    -> ChangeLens updateA updateB
    -> ChangeLens (PartialUpdate updateA) (PartialUpdate updateB)
liftPartialChangeLens maprs lens = partialiseChangeLens maprs $ partialChangeLens . lens

partialConvertChangeLens ::
       forall updateA updateB.
       ( UpdateSubject updateA ~ UpdateSubject updateB
       , FullEdit (UpdateEdit updateA)
       , SubjectMapEdit (UpdateEdit updateB)
       )
    => ChangeLens updateA (PartialUpdate updateB)
partialConvertChangeLens = let
    clRead :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    clRead mr = mSubjectToReadable $ readableToSubject mr
    clUpdate ::
           forall m. MonadIO m
        => updateA
        -> Readable m (UpdateReader updateA)
        -> m [PartialUpdate updateB]
    clUpdate _ _ = return [UnknownPartialUpdate $ \_ -> True]
    clPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit updateB]
        -> Readable m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    clPutEdits editbs mr = do
        oldsubject <- readableToSubject mr
        newsubject <- mapSubjectEdits editbs oldsubject
        editas <- getReplaceEditsFromSubject newsubject
        return $ Just editas
    in MkChangeLens {..}
