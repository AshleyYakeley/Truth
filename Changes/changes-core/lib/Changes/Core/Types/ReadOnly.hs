module Changes.Core.Types.ReadOnly where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.None

newtype ReadOnlyUpdate update = MkReadOnlyUpdate
    { unReadOnlyUpdate :: update
    }

type instance UpdateEdit (ReadOnlyUpdate update) = ConstEdit (UpdateReader update)

instance IsUpdate (ReadOnlyUpdate update) where
    editUpdate = never

instance FullUpdate update => FullUpdate (ReadOnlyUpdate update) where
    replaceUpdate rd pushU = replaceUpdate rd $ \update -> pushU $ MkReadOnlyUpdate update

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
       ( FullSubjectReader (UpdateReader updateA)
       , SubjectReader (UpdateReader updateB)
       , FullUpdate updateB
       , Empty (UpdateEdit updateB)
       )
    => (UpdateSubject updateA -> IO (UpdateSubject updateB))
    -> ChangeLens updateA updateB
ioFuncChangeLens amb = let
    clRead :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    clRead mra rt = (mSubjectToReadable $ readableToSubject mra >>= \a -> liftIO (amb a)) rt
    clUpdate ::
           forall m. MonadIO m
        => updateA
        -> Readable m (UpdateReader updateA)
        -> m [updateB]
    clUpdate _ mra =
        getReplaceUpdates $
        mSubjectToReadable $ do
            a <- readableToSubject mra
            liftIO $ amb a
    in MkChangeLens {clPutEdits = clPutEditsNone, ..}

funcChangeLens ::
       forall updateA updateB.
       ( FullSubjectReader (UpdateReader updateA)
       , SubjectReader (UpdateReader updateB)
       , FullUpdate updateB
       , Empty (UpdateEdit updateB)
       )
    => (UpdateSubject updateA -> UpdateSubject updateB)
    -> ChangeLens updateA updateB
funcChangeLens ab = ioFuncChangeLens $ \a -> return $ ab a

convertReadOnlyChangeLens ::
       forall updateA updateB.
       ( FullSubjectReader (UpdateReader updateA)
       , SubjectReader (UpdateReader updateB)
       , FullUpdate updateB
       , Empty (UpdateEdit updateB)
       , UpdateSubject updateA ~ UpdateSubject updateB
       )
    => ChangeLens updateA updateB
convertReadOnlyChangeLens = funcChangeLens id

immutableChangeLens ::
       forall updateA updateB. (Empty (UpdateEdit updateB))
    => (forall m. MonadIO m => Readable m (UpdateReader updateB))
    -> ChangeLens updateA updateB
immutableChangeLens mr = let
    clRead :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    clRead _ = mr
    clUpdate ::
           forall m. MonadIO m
        => updateA
        -> Readable m (UpdateReader updateA)
        -> m [updateB]
    clUpdate _ _ = return []
    in MkChangeLens {clPutEdits = clPutEditsNone, ..}

ioConstChangeLens ::
       (Empty (UpdateEdit updateB))
    => SubjectReader (UpdateReader updateB) => IO (UpdateSubject updateB) -> ChangeLens updateA updateB
ioConstChangeLens iob = immutableChangeLens $ mSubjectToReadable $ liftIO iob

constChangeLens ::
       (Empty (UpdateEdit updateB))
    => SubjectReader (UpdateReader updateB) => UpdateSubject updateB -> ChangeLens updateA updateB
constChangeLens b = ioConstChangeLens $ return b
