module Truth.Core.Types.SumWhole where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;
    import Truth.Core.Types.Sum;


    type SumWholeReaderEdit reader edit = SumEdit (WholeReaderEdit reader) edit;
    type SumWholeEdit edit = SumWholeReaderEdit (EditReader edit) edit;

    sumWholeLiftEditFunction :: forall c state edita editb. (ReadableConstraint c,Reader (EditReader edita), FullReader c (EditReader editb)) =>
     EditFunction c state edita editb ->
     EditFunction c state (SumWholeEdit edita) (SumWholeEdit editb);
    sumWholeLiftEditFunction fef = MkEditFunction
    {
        editInitial = editInitial fef,
        editGet = editGet fef,
        editUpdate = \pedita oldstate -> case pedita of
        {
            SumEditLeft (MkWholeEdit a) -> do
            {
                b <- case selfReadable @c @(EditReader edita) of
                {
                    MkConstraintWitness -> fromReadFunctionM (editGet fef oldstate) $ return a;
                };
                return (oldstate,return $ SumEditLeft $ MkWholeEdit b); -- state unchanged, kind of dubious
            };
            SumEditRight edita -> do
            {
                (newstate,meditb) <- editUpdate fef edita oldstate;
                return (newstate,fmap SumEditRight meditb);
            };
        }
    };

    sumWholeLiftEditLens :: (ReadableConstraint c,Functor f,Reader (EditReader edita),FullReader c (EditReader editb)) =>
     (state -> EditSubject editb -> Readable c (EditReader edita) (f (state,EditSubject edita))) ->
     EditLens' c f state edita editb ->
     EditLens' c f state (SumWholeEdit edita) (SumWholeEdit editb);
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
