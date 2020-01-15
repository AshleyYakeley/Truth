module Truth.Core.Edit.Partial
    ( ReaderSet
    , PartialUpdate(..)
    , partialFullEditLens
    , convertUpdateEditLens
    , partialEditLens
    , comapUpdateUpdateFunction
    , comapUpdateEditLens
    , partialiseUpdateFunction
    , partialiseEditLens
    , liftPartialEditLens
    , partialConvertUpdateFunction
    , partialConvertEditLens
    ) where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.FullEdit
import Truth.Core.Edit.Function
import Truth.Core.Edit.Lens
import Truth.Core.Edit.Update
import Truth.Core.Import
import Truth.Core.Read

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
    ufGet ::
           forall m t. MonadIO m
        => MutableRead m (UpdateReader update)
        -> UpdateReader update t
        -> m t
    ufGet mr = mr
    ufUpdate ::
           forall m. MonadIO m
        => PartialUpdate update
        -> MutableRead m (UpdateReader update)
        -> m [update]
    ufUpdate (KnownPartialUpdate update) _ = return [update]
    ufUpdate (UnknownPartialUpdate _) mr = do
        edits <- getReplaceEdits mr
        return $ fmap editUpdate edits
    elFunction :: UpdateFunction (PartialUpdate update) update
    elFunction = MkUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> MutableRead m (UpdateReader update)
        -> m (Maybe [UpdateEdit update])
    elPutEdits edits _ = return $ Just edits
    in MkEditLens {..}

convertUpdateEditLens ::
       forall updateA updateB. UpdateEdit updateA ~ UpdateEdit updateB
    => (updateA -> updateB)
    -> EditLens updateA updateB
convertUpdateEditLens ab = let
    ufGet ::
           forall m t. MonadIO m
        => MutableRead m (UpdateReader updateA)
        -> UpdateReader updateB t
        -> m t
    ufGet mr = mr
    ufUpdate ::
           forall m. MonadIO m
        => updateA
        -> MutableRead m (UpdateReader updateA)
        -> m [updateB]
    ufUpdate update _ = return [ab update]
    elFunction :: UpdateFunction updateA updateB
    elFunction = MkUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit updateB]
        -> MutableRead m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    elPutEdits edits _ = return $ Just edits
    in MkEditLens {..}

partialEditLens :: forall update. EditLens update (PartialUpdate update)
partialEditLens = convertUpdateEditLens KnownPartialUpdate

comapUpdateAnUpdateFunction ::
       forall updateA updateB updateC.
       (updateB -> Either updateC updateA)
    -> UpdateEdit updateA ~ UpdateEdit updateB => UpdateFunction updateA updateC -> UpdateFunction updateB updateC
comapUpdateAnUpdateFunction bca (MkUpdateFunction g u) = let
    u' :: forall m. MonadIO m
       => updateB
       -> MutableRead m (UpdateReader updateB)
       -> m [updateC]
    u' ub mr =
        case bca ub of
            Left uc -> return [uc]
            Right ua -> u ua mr
    in MkUpdateFunction g u'

comapUpdateUpdateFunction ::
       forall updateA updateB updateC.
       (updateB -> Either updateC updateA)
    -> UpdateEdit updateA ~ UpdateEdit updateB => UpdateFunction updateA updateC -> UpdateFunction updateB updateC
comapUpdateUpdateFunction = comapUpdateAnUpdateFunction

comapUpdateEditLens ::
       forall updateA updateB updateC.
       (updateB -> Either updateC updateA)
    -> UpdateEdit updateA ~ UpdateEdit updateB => EditLens updateA updateC -> EditLens updateB updateC
comapUpdateEditLens bca (MkEditLens auf p) = MkEditLens (comapUpdateAnUpdateFunction bca auf) p

partialiseUpdateFunction ::
       forall updateA updateB.
       (ReaderSet (UpdateReader updateA) -> ReaderSet (UpdateReader updateB))
    -> UpdateFunction updateA (PartialUpdate updateB)
    -> UpdateFunction (PartialUpdate updateA) (PartialUpdate updateB)
partialiseUpdateFunction maprs =
    comapUpdateUpdateFunction $ \case
        UnknownPartialUpdate upd -> Left $ UnknownPartialUpdate $ maprs upd
        KnownPartialUpdate updateA -> Right updateA

partialiseEditLens ::
       forall updateA updateB.
       (ReaderSet (UpdateReader updateA) -> ReaderSet (UpdateReader updateB))
    -> EditLens updateA (PartialUpdate updateB)
    -> EditLens (PartialUpdate updateA) (PartialUpdate updateB)
partialiseEditLens maprs =
    comapUpdateEditLens $ \case
        UnknownPartialUpdate upd -> Left $ UnknownPartialUpdate $ maprs upd
        KnownPartialUpdate updateA -> Right updateA

liftPartialEditLens ::
       forall updateA updateB.
       (ReaderSet (UpdateReader updateA) -> ReaderSet (UpdateReader updateB))
    -> EditLens updateA updateB
    -> EditLens (PartialUpdate updateA) (PartialUpdate updateB)
liftPartialEditLens maprs lens = partialiseEditLens maprs $ partialEditLens . lens

partialConvertAnUpdateFunction ::
       forall updateA updateB.
       ( IsEditUpdate updateA
       , IsUpdate updateB
       , UpdateSubject updateA ~ UpdateSubject updateB
       , FullSubjectReader (UpdateReader updateA)
       , ApplicableEdit (UpdateEdit updateA)
       , SubjectReader (UpdateReader updateB)
       )
    => UpdateFunction updateA (PartialUpdate updateB)
partialConvertAnUpdateFunction = let
    ufGet :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    ufGet mr = mSubjectToMutableRead $ mutableReadToSubject mr
    ufUpdate ::
           forall m. MonadIO m
        => updateA
        -> MutableRead m (UpdateReader updateA)
        -> m [PartialUpdate updateB]
    ufUpdate _ _ = return [UnknownPartialUpdate $ \_ -> True]
    in MkUpdateFunction {..}

partialConvertUpdateFunction ::
       forall updateA updateB.
       ( IsEditUpdate updateA
       , IsUpdate updateB
       , UpdateSubject updateA ~ UpdateSubject updateB
       , FullSubjectReader (UpdateReader updateA)
       , ApplicableEdit (UpdateEdit updateA)
       , SubjectReader (UpdateReader updateB)
       )
    => UpdateFunction updateA (PartialUpdate updateB)
partialConvertUpdateFunction = partialConvertAnUpdateFunction

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
    elFunction :: UpdateFunction updateA (PartialUpdate updateB)
    elFunction = partialConvertAnUpdateFunction
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit updateB]
        -> MutableRead m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    elPutEdits editbs mr = do
        oldsubject <- mutableReadToSubject mr
        newsubject <- mapSubjectEdits editbs oldsubject
        editas <- getReplaceEditsFromSubject newsubject
        return $ Just editas
    in MkEditLens {..}
