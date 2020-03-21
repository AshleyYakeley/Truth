module Truth.Core.Types.SumWhole where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.Sum
import Truth.Core.Types.Whole

type SumWholeReaderEdit reader edit = SumEdit (WholeReaderEdit reader) edit

type SumWholeEdit edit = SumWholeReaderEdit (EditReader edit) edit

type SumWholeReaderUpdate reader update = SumUpdate (WholeReaderUpdate reader) update

type SumWholeUpdate update = SumWholeReaderUpdate (UpdateReader update) update

sumWholeLiftEditLens ::
       forall updateA updateB.
       ( ApplicableEdit (UpdateEdit updateA)
       , FullSubjectReader (UpdateReader updateA)
       , FullSubjectReader (UpdateReader updateB)
       )
    => (forall m.
            MonadIO m =>
                    UpdateSubject updateB -> MutableRead m (UpdateReader updateA) -> m (Maybe (UpdateSubject updateA)))
    -> EditLens updateA updateB
    -> EditLens (SumWholeUpdate updateA) (SumWholeUpdate updateB)
sumWholeLiftEditLens pushback (MkEditLens g u pe) = let
    elUpdate ::
           forall m. MonadIO m
        => SumWholeUpdate updateA
        -> MutableRead m (UpdateReader updateA)
        -> m [SumWholeUpdate updateB]
    elUpdate pupdatea mr =
        case pupdatea of
            SumUpdateLeft (MkWholeReaderUpdate a) -> do
                b <- mutableReadToSubject $ g $ subjectToMutableRead @m a
                return [SumUpdateLeft $ MkWholeReaderUpdate b]
            SumUpdateRight edita -> do
                editbs <- u edita mr
                return $ fmap SumUpdateRight editbs
    elPutEdit ::
           forall m. MonadIO m
        => SumEdit (WholeReaderEdit (UpdateReader updateB)) (UpdateEdit updateB)
        -> MutableRead m (UpdateReader updateA)
        -> m (Maybe [SumEdit (WholeReaderEdit (UpdateReader updateA)) (UpdateEdit updateA)])
    elPutEdit peditb mr =
        case peditb of
            SumEditLeft (MkWholeReaderEdit b) -> do
                ma <- pushback b mr
                return $ fmap (pure . SumEditLeft . MkWholeReaderEdit) ma
            SumEditRight editb -> do
                mstateedita <- pe [editb] mr
                return $ fmap (fmap SumEditRight) mstateedita
    in MkEditLens {elPutEdits = elPutEditsFromPutEdit elPutEdit, elGet = g, ..}
