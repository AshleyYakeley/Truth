module Changes.Core.Types.ReadOnly where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.None

newtype ReadOnlyUpdate update = MkReadOnlyUpdate
    { unReadOnlyUpdate :: update
    }

type instance UpdateEdit (ReadOnlyUpdate update) =
     ConstEdit (UpdateReader update)

instance IsUpdate (ReadOnlyUpdate update) where
    editUpdate = never

toReadOnlyChangeLens :: forall update. ChangeLens update (ReadOnlyUpdate update)
toReadOnlyChangeLens = let
    clRead :: ReadFunction (UpdateReader update) (UpdateReader update)
    clRead mr = mr
    clUpdate ::
           forall m. MonadIO m
        => update
        -> Readable m (UpdateReader update)
        -> m [ReadOnlyUpdate update]
    clUpdate update _ = return [MkReadOnlyUpdate update]
    in MkChangeLens {clPutEdits = clPutEditsNone, ..}

liftReadOnlyChangeLens ::
       forall updateA updateB.
       ChangeLens updateA (ReadOnlyUpdate updateB)
    -> ChangeLens (ReadOnlyUpdate updateA) (ReadOnlyUpdate updateB)
liftReadOnlyChangeLens (MkChangeLens g u _) = let
    u' :: forall m. MonadIO m
       => ReadOnlyUpdate updateA
       -> Readable m (UpdateReader updateA)
       -> m [ReadOnlyUpdate updateB]
    u' (MkReadOnlyUpdate updA) mr = u updA mr
    in MkChangeLens g u' clPutEditsNone

liftReadOnlyFloatingChangeLens ::
       forall updateA updateB.
       FloatingChangeLens updateA (ReadOnlyUpdate updateB)
    -> FloatingChangeLens (ReadOnlyUpdate updateA) (ReadOnlyUpdate updateB)
liftReadOnlyFloatingChangeLens = floatLift (\mr -> mr) liftReadOnlyChangeLens

ioFuncChangeLens ::
       forall updateA updateB.
       (IsUpdate updateB, FullSubjectReader (UpdateReader updateA), FullEdit (UpdateEdit updateB))
    => (UpdateSubject updateA -> IO (UpdateSubject updateB))
    -> ChangeLens updateA (ReadOnlyUpdate updateB)
ioFuncChangeLens amb = let
    clRead :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    clRead mra rt = (mSubjectToReadable $ readableToSubject mra >>= \a -> liftIO (amb a)) rt
    clUpdate ::
           forall m. MonadIO m
        => updateA
        -> Readable m (UpdateReader updateA)
        -> m [ReadOnlyUpdate updateB]
    clUpdate _ mra =
        fmap (fmap (MkReadOnlyUpdate . editUpdate)) $
        getReplaceEdits $
        mSubjectToReadable $ do
            a <- readableToSubject mra
            liftIO $ amb a
    clPutEdits ::
           forall m. MonadIO m
        => [ConstEdit (UpdateReader updateB)]
        -> Readable m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    clPutEdits = clPutEditsNone
    in MkChangeLens {..}

funcChangeLens ::
       forall updateA updateB.
       (IsUpdate updateB, FullSubjectReader (UpdateReader updateA), FullEdit (UpdateEdit updateB))
    => (UpdateSubject updateA -> UpdateSubject updateB)
    -> ChangeLens updateA (ReadOnlyUpdate updateB)
funcChangeLens ab = ioFuncChangeLens $ \a -> return $ ab a

convertReadOnlyChangeLens ::
       forall updateA updateB.
       ( IsUpdate updateB
       , FullSubjectReader (UpdateReader updateA)
       , FullEdit (UpdateEdit updateB)
       , UpdateSubject updateA ~ UpdateSubject updateB
       )
    => ChangeLens updateA (ReadOnlyUpdate updateB)
convertReadOnlyChangeLens = funcChangeLens id

immutableChangeLens ::
       forall updateA updateB.
       (forall m. MonadIO m => Readable m (UpdateReader updateB))
    -> ChangeLens updateA (ReadOnlyUpdate updateB)
immutableChangeLens mr = let
    clRead :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    clRead _ = mr
    clUpdate ::
           forall m. MonadIO m
        => updateA
        -> Readable m (UpdateReader updateA)
        -> m [ReadOnlyUpdate updateB]
    clUpdate _ _ = return []
    clPutEdits ::
           forall m. MonadIO m
        => [ConstEdit (UpdateReader updateB)]
        -> Readable m (UpdateReader updateA)
        -> m (Maybe [UpdateEdit updateA])
    clPutEdits = clPutEditsNone
    in MkChangeLens {..}

ioConstChangeLens ::
       SubjectReader (UpdateReader updateB) => IO (UpdateSubject updateB) -> ChangeLens updateA (ReadOnlyUpdate updateB)
ioConstChangeLens iob = immutableChangeLens $ mSubjectToReadable $ liftIO iob

constChangeLens ::
       SubjectReader (UpdateReader updateB) => UpdateSubject updateB -> ChangeLens updateA (ReadOnlyUpdate updateB)
constChangeLens b = ioConstChangeLens $ return b
