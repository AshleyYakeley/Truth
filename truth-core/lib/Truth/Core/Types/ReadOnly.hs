module Truth.Core.Types.ReadOnly where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.None

newtype ReadOnlyUpdate update = MkReadOnlyUpdate
    { unReadOnlyUpdate :: update
    }

type instance UpdateEdit (ReadOnlyUpdate update) =
     ConstEdit (UpdateReader update)

instance IsUpdate (ReadOnlyUpdate update) where
    editUpdate = never

toReadOnlyEditLens :: forall update. EditLens update (ReadOnlyUpdate update)
toReadOnlyEditLens = let
    elGet :: ReadFunction (UpdateReader update) (UpdateReader update)
    elGet mr = mr
    elUpdate ::
           forall m. MonadIO m
        => update
        -> MutableRead m (UpdateReader update)
        -> m [ReadOnlyUpdate update]
    elUpdate update _ = return [MkReadOnlyUpdate update]
    in MkEditLens {elPutEdits = elPutEditsNone, ..}

liftReadOnlyEditLens ::
       forall updateA updateB.
       EditLens updateA (ReadOnlyUpdate updateB)
    -> EditLens (ReadOnlyUpdate updateA) (ReadOnlyUpdate updateB)
liftReadOnlyEditLens (MkEditLens g u _) = let
    u' :: forall m. MonadIO m
       => ReadOnlyUpdate updateA
       -> MutableRead m (UpdateReader updateA)
       -> m [ReadOnlyUpdate updateB]
    u' (MkReadOnlyUpdate updA) mr = u updA mr
    in MkEditLens g u' elPutEditsNone

liftReadOnlyFloatingEditLens ::
       forall updateA updateB.
       FloatingEditLens updateA (ReadOnlyUpdate updateB)
    -> FloatingEditLens (ReadOnlyUpdate updateA) (ReadOnlyUpdate updateB)
liftReadOnlyFloatingEditLens = floatLift (\mr -> mr) liftReadOnlyEditLens

ioFuncEditLens ::
       forall updateA updateB.
       (IsUpdate updateB, FullSubjectReader (UpdateReader updateA), FullEdit (UpdateEdit updateB))
    => (UpdateSubject updateA -> IO (UpdateSubject updateB))
    -> EditLens updateA (ReadOnlyUpdate updateB)
ioFuncEditLens amb = let
    elGet :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    elGet mra rt = (mSubjectToMutableRead $ mutableReadToSubject mra >>= \a -> liftIO (amb a)) rt
    elUpdate ::
           forall m. MonadIO m
        => updateA
        -> MutableRead m (UpdateReader updateA)
        -> m [ReadOnlyUpdate updateB]
    elUpdate _ mra =
        fmap (fmap (MkReadOnlyUpdate . editUpdate)) $
        getReplaceEdits $
        mSubjectToMutableRead $ do
            a <- mutableReadToSubject mra
            liftIO $ amb a
    elPutEdits ::
           forall m. MonadIO m
        => [ConstEdit (UpdateReader updateB)]
        -> MutableRead m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    elPutEdits = elPutEditsNone
    in MkEditLens {..}

funcEditLens ::
       forall updateA updateB.
       (IsUpdate updateB, FullSubjectReader (UpdateReader updateA), FullEdit (UpdateEdit updateB))
    => (UpdateSubject updateA -> UpdateSubject updateB)
    -> EditLens updateA (ReadOnlyUpdate updateB)
funcEditLens ab = ioFuncEditLens $ \a -> return $ ab a

convertReadOnlyEditLens ::
       forall updateA updateB.
       ( IsUpdate updateB
       , FullSubjectReader (UpdateReader updateA)
       , FullEdit (UpdateEdit updateB)
       , UpdateSubject updateA ~ UpdateSubject updateB
       )
    => EditLens updateA (ReadOnlyUpdate updateB)
convertReadOnlyEditLens = funcEditLens id

immutableEditLens ::
       forall updateA updateB.
       (forall m. MonadIO m => MutableRead m (UpdateReader updateB))
    -> EditLens updateA (ReadOnlyUpdate updateB)
immutableEditLens mr = let
    elGet :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    elGet _ = mr
    elUpdate ::
           forall m. MonadIO m
        => updateA
        -> MutableRead m (UpdateReader updateA)
        -> m [ReadOnlyUpdate updateB]
    elUpdate _ _ = return []
    elPutEdits ::
           forall m. MonadIO m
        => [ConstEdit (UpdateReader updateB)]
        -> MutableRead m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    elPutEdits = elPutEditsNone
    in MkEditLens {..}

ioConstEditLens ::
       SubjectReader (UpdateReader updateB) => IO (UpdateSubject updateB) -> EditLens updateA (ReadOnlyUpdate updateB)
ioConstEditLens iob = immutableEditLens $ mSubjectToMutableRead $ liftIO iob

constEditLens ::
       SubjectReader (UpdateReader updateB) => UpdateSubject updateB -> EditLens updateA (ReadOnlyUpdate updateB)
constEditLens b = ioConstEditLens $ return b
