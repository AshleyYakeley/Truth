module Changes.Core.Types.Unit where

import Changes.Core.Edit
import Changes.Core.Lens
import Changes.Core.Read
import Changes.Core.Types.None
import Changes.Core.Types.Whole
import Shapes

type ConstWholeEdit a = ConstEdit (WholeReader a)

type ConstWholeUpdate a = ConstUpdate (WholeReader a)

type UnitReader = WholeReader ()

type UnitEdit = ConstWholeEdit ()

type UnitUpdate = ConstWholeUpdate ()

ioConstWholeChangeLens ::
       forall updateA updateB.
       ( Empty updateA
       , FullSubjectReader (UpdateReader updateA)
       , Empty (UpdateEdit updateB)
       , SubjectReader (UpdateReader updateB)
       )
    => (UpdateSubject updateA -> IO (UpdateSubject updateB))
    -> ChangeLens updateA updateB
ioConstWholeChangeLens amb = let
    clRead :: ReadFunction (UpdateReader updateA) (UpdateReader updateB)
    clRead mra rt = (mSubjectToReadable $ readableToSubject mra >>= \a -> liftIO (amb a)) rt
    clUpdate :: forall m. updateA -> Readable m (UpdateReader updateA) -> m [updateB]
    clUpdate u _ = never u
    in MkChangeLens {clPutEdits = clPutEditsNone, ..}

constWholeChangeLens ::
       forall updateA updateB.
       ( Empty updateA
       , FullSubjectReader (UpdateReader updateA)
       , Empty (UpdateEdit updateB)
       , SubjectReader (UpdateReader updateB)
       )
    => (UpdateSubject updateA -> UpdateSubject updateB)
    -> ChangeLens updateA updateB
constWholeChangeLens ab = ioConstWholeChangeLens $ \a -> return $ ab a
