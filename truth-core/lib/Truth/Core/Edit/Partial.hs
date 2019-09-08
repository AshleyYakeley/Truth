module Truth.Core.Edit.Partial
    ( ReaderSet
    , PartialUpdate(..)
    , partialFullEditLens
    , convertUpdateEditLens
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
import Truth.Core.Edit.Unlift
import Truth.Core.Edit.Update
import Truth.Core.Import
import Truth.Core.Read

type ReaderSet reader = forall t. reader t -> Bool

data PartialUpdate update
    = KnownPartialUpdate update
    | UnknownPartialUpdate (ReaderSet (UpdateReader update))

instance IsUpdate update => IsUpdate (PartialUpdate update) where
    type UpdateEdit (PartialUpdate update) = UpdateEdit update
    editUpdate edit = KnownPartialUpdate $ editUpdate edit

partialFullEditLens ::
       forall update. (IsUpdate update, FullEdit (UpdateEdit update))
    => EditLens (PartialUpdate update) update
partialFullEditLens = let
    ufGet ::
           forall m t. MonadIO m
        => MutableRead m (UpdateReader update)
        -> UpdateReader update t
        -> IdentityT m t
    ufGet mr rt = lift $ mr rt
    ufUpdate ::
           forall m. MonadIO m
        => PartialUpdate update
        -> MutableRead m (UpdateReader update)
        -> IdentityT m [update]
    ufUpdate (KnownPartialUpdate update) _ = return [update]
    ufUpdate (UnknownPartialUpdate _) mr =
        lift $ do
            edits <- getReplaceEdits mr
            return $ fmap editUpdate edits
    elFunction :: AnUpdateFunction IdentityT (PartialUpdate update) update
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit update]
        -> MutableRead m (UpdateReader update)
        -> IdentityT m (Maybe [UpdateEdit update])
    elPutEdits edits _ = return $ Just edits
    in MkCloseUnlift identityUnlift MkAnEditLens {..}

convertUpdateEditLens ::
       forall updateA updateB. UpdateEdit updateA ~ UpdateEdit updateB
    => (updateA -> updateB)
    -> EditLens updateA updateB
convertUpdateEditLens ab = let
    ufGet ::
           forall m t. MonadIO m
        => MutableRead m (UpdateReader updateA)
        -> UpdateReader updateB t
        -> IdentityT m t
    ufGet mr rt = lift $ mr rt
    ufUpdate ::
           forall m. MonadIO m
        => updateA
        -> MutableRead m (UpdateReader updateA)
        -> IdentityT m [updateB]
    ufUpdate update _ = return [ab update]
    elFunction :: AnUpdateFunction IdentityT updateA updateB
    elFunction = MkAnUpdateFunction {..}
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit updateB]
        -> MutableRead m (UpdateReader updateA)
        -> IdentityT m (Maybe [UpdateEdit updateA])
    elPutEdits edits _ = return $ Just edits
    in MkCloseUnlift identityUnlift MkAnEditLens {..}

partialEditLens :: forall update. EditLens update (PartialUpdate update)
partialEditLens = convertUpdateEditLens KnownPartialUpdate

comapUpdateAnUpdateFunction ::
       forall t updateA updateB updateC. MonadTrans t
    => (updateB -> Either updateC updateA)
    -> UpdateEdit updateA ~ UpdateEdit updateB =>
               AnUpdateFunction t updateA updateC -> AnUpdateFunction t updateB updateC
comapUpdateAnUpdateFunction bca (MkAnUpdateFunction g u) = let
    u' :: forall m. MonadIO m
       => updateB
       -> MutableRead m (UpdateReader updateB)
       -> t m [updateC]
    u' ub mr =
        case bca ub of
            Left uc -> lift $ return [uc]
            Right ua -> u ua mr
    in MkAnUpdateFunction g u'

comapUpdateUpdateFunction ::
       forall updateA updateB updateC.
       (updateB -> Either updateC updateA)
    -> UpdateEdit updateA ~ UpdateEdit updateB => UpdateFunction updateA updateC -> UpdateFunction updateB updateC
comapUpdateUpdateFunction bca (MkCloseUnlift unlift auf) = MkCloseUnlift unlift $ comapUpdateAnUpdateFunction bca auf

comapUpdateEditLens ::
       forall updateA updateB updateC.
       (updateB -> Either updateC updateA)
    -> UpdateEdit updateA ~ UpdateEdit updateB => EditLens updateA updateC -> EditLens updateB updateC
comapUpdateEditLens bca (MkCloseUnlift unlift (MkAnEditLens auf p)) =
    MkCloseUnlift unlift $ MkAnEditLens (comapUpdateAnUpdateFunction bca auf) p

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
    => AnUpdateFunction IdentityT updateA (PartialUpdate updateB)
partialConvertAnUpdateFunction = let
    ufGet :: ReadFunctionT IdentityT (UpdateReader updateA) (UpdateReader updateB)
    ufGet mr = mSubjectToMutableRead $ lift $ mutableReadToSubject mr
    ufUpdate ::
           forall m. MonadIO m
        => updateA
        -> MutableRead m (UpdateReader updateA)
        -> IdentityT m [PartialUpdate updateB]
    ufUpdate _ _ = return [UnknownPartialUpdate $ \_ -> True]
    in MkAnUpdateFunction {..}

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
partialConvertUpdateFunction = MkCloseUnlift identityUnlift partialConvertAnUpdateFunction

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
    elFunction :: AnUpdateFunction IdentityT updateA (PartialUpdate updateB)
    elFunction = partialConvertAnUpdateFunction
    elPutEdits ::
           forall m. MonadIO m
        => [UpdateEdit updateB]
        -> MutableRead m (UpdateReader updateA)
        -> IdentityT m (Maybe [UpdateEdit updateA])
    elPutEdits editbs mr = do
        oldsubject <- lift $ mutableReadToSubject mr
        newsubject <- mapSubjectEdits editbs oldsubject
        editas <- getReplaceEditsFromSubject newsubject
        return $ Just editas
    in MkCloseUnlift identityUnlift MkAnEditLens {..}
