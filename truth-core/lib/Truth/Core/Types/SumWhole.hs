module Truth.Core.Types.SumWhole where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;
    import Truth.Core.Types.Sum;


    type SumWholeReaderEdit reader edit = SumEdit (WholeReaderEdit reader) edit;
    type SumWholeEdit edit = SumWholeReaderEdit (EditReader edit) edit;

    sumWholeLiftEditFunction :: forall state edita editb. (SubjectReader (EditReader edita), FullSubjectReader (EditReader editb)) =>
     EditFunction state edita editb ->
     EditFunction state (SumWholeEdit edita) (SumWholeEdit editb);
    sumWholeLiftEditFunction fef = MkEditFunction
    {
        editAccess = editAccess fef,
        editGet = editGet fef,
        editUpdate = \pedita oldstate -> case pedita of
        {
            SumEditLeft (MkWholeEdit a) -> do
            {
                b <- fromReadFunctionM (editGet fef oldstate) $ return a;
                return (oldstate,return $ SumEditLeft $ MkWholeEdit b); -- state unchanged, kind of dubious
            };
            SumEditRight edita -> do
            {
                (newstate,meditb) <- editUpdate fef edita oldstate;
                return (newstate,fmap SumEditRight meditb);
            };
        }
    };

    sumWholeLiftEditLens :: (SubjectReader (EditReader edita),FullSubjectReader (EditReader editb)) =>
     (state -> EditSubject editb -> Readable (EditReader edita) (Maybe (state,EditSubject edita))) ->
     EditLens state edita editb ->
     EditLens state (SumWholeEdit edita) (SumWholeEdit editb);
    sumWholeLiftEditLens pushback lens = MkEditLens
    {
        editLensFunction = sumWholeLiftEditFunction (editLensFunction lens),
        editLensPutEdit = \st peditb -> case peditb of
        {
            SumEditLeft (MkWholeEdit b) -> do
            {
                ma <- pushback st b;
                return $ fmap (fmap (pure . SumEditLeft . MkWholeEdit)) ma;
            };
            SumEditRight editb -> do
            {
                mstateedita <- editLensPutEdit lens st editb;
                return $ fmap (fmap (fmap SumEditRight)) mstateedita;
            };
        }
    };
}
