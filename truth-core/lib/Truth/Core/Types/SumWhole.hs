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
       forall t updateA updateB.
       (MonadTransConstraint MonadIO t, SubjectReader (UpdateReader updateA), FullSubjectReader (UpdateReader updateB))
    => AnUpdateFunction t updateA updateB
    -> AnUpdateFunction t (SumWholeUpdate updateA) (SumWholeUpdate updateB)
sumWholeLiftAnUpdateFunction fef =
    MkAnUpdateFunction
        { ufGet = ufGet fef
        , ufUpdate =
              \pupdatea mr ->
                  withTransConstraintTM @MonadIO $
                  case pupdatea of
                      SumUpdateLeft (MkWholeReaderUpdate a) -> do
                          b <- mutableReadToSubject $ ufGet fef $ subjectToMutableRead a
                          return [SumUpdateLeft $ MkWholeReaderUpdate b]
                      SumUpdateRight edita -> do
                          editbs <- ufUpdate fef edita mr
                          return $ fmap SumUpdateRight editbs
        }

sumWholeLiftUpdateFunction ::
       forall updateA updateB. (SubjectReader (UpdateReader updateA), FullSubjectReader (UpdateReader updateB))
    => UpdateFunction updateA updateB
    -> UpdateFunction (SumWholeUpdate updateA) (SumWholeUpdate updateB)
sumWholeLiftUpdateFunction (MkRunnableT2 unlift f) = MkRunnableT2 unlift $ sumWholeLiftAnUpdateFunction f

sumWholeLiftAnEditLens ::
       forall t updateA updateB.
       ( MonadTransConstraint MonadIO t
       , ApplicableEdit (UpdateEdit updateA)
       , FullSubjectReader (UpdateReader updateA)
       , FullSubjectReader (UpdateReader updateB)
       )
    => (forall m.
            MonadIO m =>
                    UpdateSubject updateB -> MutableRead m (UpdateReader updateA) -> t m (Maybe (UpdateSubject updateA)))
    -> AnEditLens t updateA updateB
    -> AnEditLens t (SumWholeUpdate updateA) (SumWholeUpdate updateB)
sumWholeLiftAnEditLens pushback lens = let
    elPutEdit ::
           forall m. MonadIO m
        => SumEdit (WholeReaderEdit (UpdateReader updateB)) (UpdateEdit updateB)
        -> MutableRead m (UpdateReader updateA)
        -> t m (Maybe [SumEdit (WholeReaderEdit (UpdateReader updateA)) (UpdateEdit updateA)])
    elPutEdit peditb mr =
        withTransConstraintTM @MonadIO $
        case peditb of
            SumEditLeft (MkWholeReaderEdit b) -> do
                ma <- pushback b mr
                return $ fmap (pure . SumEditLeft . MkWholeReaderEdit) ma
            SumEditRight editb -> do
                mstateedita <- elPutEdits lens [editb] mr
                return $ fmap (fmap SumEditRight) mstateedita
    in MkAnEditLens
           {elFunction = sumWholeLiftAnUpdateFunction (elFunction lens), elPutEdits = elPutEditsFromPutEdit elPutEdit}
