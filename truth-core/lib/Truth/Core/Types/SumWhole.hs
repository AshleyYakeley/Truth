module Truth.Core.Types.SumWhole where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Sum
import Truth.Core.Types.Whole

type SumWholeReaderEdit reader edit = SumEdit (WholeReaderEdit reader) edit

type SumWholeEdit edit = SumWholeReaderEdit (EditReader edit) edit

sumWholeLiftAnEditFunction ::
       forall t edita editb.
       (MonadTransConstraint MonadIO t, SubjectReader (EditReader edita), FullSubjectReader (EditReader editb))
    => AnEditFunction t edita editb
    -> AnEditFunction t (SumWholeEdit edita) (SumWholeEdit editb)
sumWholeLiftAnEditFunction fef =
    MkAnEditFunction
    { efGet = efGet fef
    , efUpdate =
          \pedita mr ->
              withTransConstraintTM @MonadIO $
              case pedita of
                  SumEditLeft (MkWholeEdit a) -> do
                      b <- mutableReadToSubject $ efGet fef $ subjectToMutableRead a
                      return [SumEditLeft $ MkWholeEdit b]
                  SumEditRight edita -> do
                      editbs <- efUpdate fef edita mr
                      return $ fmap SumEditRight editbs
    }

sumWholeLiftEditFunction ::
       forall edita editb. (SubjectReader (EditReader edita), FullSubjectReader (EditReader editb))
    => EditFunction edita editb
    -> EditFunction (SumWholeEdit edita) (SumWholeEdit editb)
sumWholeLiftEditFunction (MkCloseUnlift unlift f) = MkCloseUnlift unlift $ sumWholeLiftAnEditFunction f

sumWholeLiftAnEditLens ::
       forall t edita editb.
       ( MonadTransConstraint MonadIO t
       , ApplicableEdit edita
       , FullSubjectReader (EditReader edita)
       , FullSubjectReader (EditReader editb)
       )
    => (forall m. MonadIO m =>
                      EditSubject editb -> MutableRead m (EditReader edita) -> t m (Maybe (EditSubject edita)))
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
       {elFunction = sumWholeLiftAnEditFunction (elFunction lens), elPutEdits = elPutEditsFromPutEdit elPutEdit}
