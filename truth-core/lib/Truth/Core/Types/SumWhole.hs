module Truth.Core.Types.SumWhole where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Sum
import Truth.Core.Types.Whole

type SumWholeReaderEdit reader edit = SumEdit (WholeReaderEdit reader) edit

type SumWholeEdit edit = SumWholeReaderEdit (EditReader edit) edit

sumWholeLiftAnUpdateFunction ::
       forall t edita editb.
       (MonadTransConstraint MonadIO t, SubjectReader (EditReader edita), FullSubjectReader (EditReader editb))
    => AnUpdateFunction t edita editb
    -> AnUpdateFunction t (SumWholeEdit edita) (SumWholeEdit editb)
sumWholeLiftAnUpdateFunction fef =
    MkAnUpdateFunction
        { ufGet = ufGet fef
        , ufUpdate =
              \pedita mr ->
                  withTransConstraintTM @MonadIO $
                  case pedita of
                      SumEditLeft (MkWholeEdit a) -> do
                          b <- mutableReadToSubject $ ufGet fef $ subjectToMutableRead a
                          return [SumEditLeft $ MkWholeEdit b]
                      SumEditRight edita -> do
                          editbs <- ufUpdate fef edita mr
                          return $ fmap SumEditRight editbs
        }

sumWholeLiftUpdateFunction ::
       forall edita editb. (SubjectReader (EditReader edita), FullSubjectReader (EditReader editb))
    => UpdateFunction edita editb
    -> UpdateFunction (SumWholeEdit edita) (SumWholeEdit editb)
sumWholeLiftUpdateFunction (MkCloseUnlift unlift f) = MkCloseUnlift unlift $ sumWholeLiftAnUpdateFunction f

sumWholeLiftAnEditLens ::
       forall t edita editb.
       ( MonadTransConstraint MonadIO t
       , ApplicableEdit edita
       , FullSubjectReader (EditReader edita)
       , FullSubjectReader (EditReader editb)
       )
    => (forall m. MonadIO m => EditSubject editb -> MutableRead m (EditReader edita) -> t m (Maybe (EditSubject edita)))
    -> AnEditLens t edita editb
    -> AnEditLens t (SumWholeEdit edita) (SumWholeEdit editb)
sumWholeLiftAnEditLens pushback lens = let
    elPutEdit ::
           forall m. MonadIO m
        => SumEdit (WholeReaderEdit (EditReader editb)) editb
        -> MutableRead m (EditReader edita)
        -> t m (Maybe [SumEdit (WholeReaderEdit (EditReader edita)) edita])
    elPutEdit peditb mr =
        withTransConstraintTM @MonadIO $
        case peditb of
            SumEditLeft (MkWholeEdit b) -> do
                ma <- pushback b mr
                return $ fmap (pure . SumEditLeft . MkWholeEdit) ma
            SumEditRight editb -> do
                mstateedita <- elPutEdits lens [editb] mr
                return $ fmap (fmap SumEditRight) mstateedita
    in MkAnEditLens
           {elFunction = sumWholeLiftAnUpdateFunction (elFunction lens), elPutEdits = elPutEditsFromPutEdit elPutEdit}
