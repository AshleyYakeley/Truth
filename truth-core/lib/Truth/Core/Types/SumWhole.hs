module Truth.Core.Types.SumWhole where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Sum
import Truth.Core.Types.Whole

type SumWholeReaderEdit reader edit = SumEdit (WholeReaderEdit reader) edit

type SumWholeEdit edit = SumWholeReaderEdit (EditReader edit) edit

type SumWholeReaderUpdate reader update = SumUpdate (WholeReaderUpdate reader) update

type SumWholeUpdate update = SumWholeReaderUpdate (UpdateReader update) update

sumWholeLiftAnUpdateFunction ::
       forall tt updateA updateB.
       (MonadTransStackUnliftAll tt, SubjectReader (UpdateReader updateA), FullSubjectReader (UpdateReader updateB))
    => AnUpdateFunction tt updateA updateB
    -> AnUpdateFunction tt (SumWholeUpdate updateA) (SumWholeUpdate updateB)
sumWholeLiftAnUpdateFunction (MkAnUpdateFunction g u) = let
    ufGet :: ReadFunctionTT tt _ _
    ufGet = g
    ufUpdate ::
           forall m. MonadIO m
        => SumWholeUpdate updateA
        -> MutableRead m (UpdateReader updateA)
        -> ApplyStack tt m [SumWholeUpdate updateB]
    ufUpdate pupdatea mr =
        case transStackUnliftMonadIO @tt @m of
            Dict ->
                case pupdatea of
                    SumUpdateLeft (MkWholeReaderUpdate a) -> do
                        b <- mutableReadToSubject $ g $ subjectToMutableRead @m a
                        return [SumUpdateLeft $ MkWholeReaderUpdate b]
                    SumUpdateRight edita -> do
                        editbs <- u edita mr
                        return $ fmap SumUpdateRight editbs
    in MkAnUpdateFunction {..}

sumWholeLiftUpdateFunction ::
       forall updateA updateB. (SubjectReader (UpdateReader updateA), FullSubjectReader (UpdateReader updateB))
    => UpdateFunction updateA updateB
    -> UpdateFunction (SumWholeUpdate updateA) (SumWholeUpdate updateB)
sumWholeLiftUpdateFunction (MkRunnable2 run@(MkTransStackRunner _) f) = MkRunnable2 run $ sumWholeLiftAnUpdateFunction f

sumWholeLiftAnEditLens ::
       forall tt updateA updateB.
       ( MonadTransStackUnliftAll tt
       , ApplicableEdit (UpdateEdit updateA)
       , FullSubjectReader (UpdateReader updateA)
       , FullSubjectReader (UpdateReader updateB)
       )
    => (forall m.
            MonadIO m =>
                    UpdateSubject updateB -> MutableRead m (UpdateReader updateA) -> ApplyStack tt m (Maybe (UpdateSubject updateA)))
    -> AnEditLens tt updateA updateB
    -> AnEditLens tt (SumWholeUpdate updateA) (SumWholeUpdate updateB)
sumWholeLiftAnEditLens pushback lens = let
    elPutEdit ::
           forall m. MonadIO m
        => SumEdit (WholeReaderEdit (UpdateReader updateB)) (UpdateEdit updateB)
        -> MutableRead m (UpdateReader updateA)
        -> ApplyStack tt m (Maybe [SumEdit (WholeReaderEdit (UpdateReader updateA)) (UpdateEdit updateA)])
    elPutEdit peditb mr =
        case transStackUnliftMonadIO @tt @m of
            Dict ->
                case peditb of
                    SumEditLeft (MkWholeReaderEdit b) -> do
                        ma <- pushback b mr
                        return $ fmap (pure . SumEditLeft . MkWholeReaderEdit) ma
                    SumEditRight editb -> do
                        mstateedita <- elPutEdits lens [editb] mr
                        return $ fmap (fmap SumEditRight) mstateedita
    in MkAnEditLens
           { elFunction = sumWholeLiftAnUpdateFunction (elFunction lens)
           , elPutEdits = elPutEditsFromPutEdit @tt elPutEdit
           }
