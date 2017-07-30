module Truth.Core.Types.SumWhole where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;
    import Truth.Core.Types.Sum;


    type SumWholeReaderEdit reader edit = SumEdit (WholeReaderEdit reader) edit;
    type SumWholeEdit edit = SumWholeReaderEdit (EditReader edit) edit;

    sumWholeLiftFloatingEditFunction :: forall c state edita editb. (ReadableConstraint c,Reader (EditReader edita), GenFullReader c (EditReader editb)) =>
     GenFloatingEditFunction c state edita editb ->
     GenFloatingEditFunction c state (SumWholeEdit edita) (SumWholeEdit editb);
    sumWholeLiftFloatingEditFunction fef = MkFloatingEditFunction
    {
        floatingEditInitial = floatingEditInitial fef,
        floatingEditGet = floatingEditGet fef,
        floatingEditUpdate = \pedita oldstate -> case pedita of
        {
            SumEditLeft (MkWholeEdit a) -> do
            {
                b <- case selfReadable @c @(EditReader edita) of
                {
                    MkConstraintWitness -> fromReadFunctionM (floatingEditGet fef oldstate) $ return a;
                };
                return (oldstate,return $ SumEditLeft $ MkWholeEdit b); -- state unchanged, kind of dubious
            };
            SumEditRight edita -> do
            {
                (newstate,meditb) <- floatingEditUpdate fef edita oldstate;
                return (newstate,fmap SumEditRight meditb);
            };
        }
    };

    sumWholeLiftFloatingEditLens :: (ReadableConstraint c,Functor f,Reader (EditReader edita),GenFullReader c (EditReader editb)) =>
     (state -> EditSubject editb -> GenReadable c (EditReader edita) (f (state,EditSubject edita))) ->
     GenFloatingEditLens' c f state edita editb ->
     GenFloatingEditLens' c f state (SumWholeEdit edita) (SumWholeEdit editb);
    sumWholeLiftFloatingEditLens pushback lens = MkFloatingEditLens
    {
        floatingEditLensFunction = sumWholeLiftFloatingEditFunction (floatingEditLensFunction lens),
        floatingEditLensPutEdit = \st peditb -> case peditb of
        {
            SumEditLeft (MkWholeEdit b) -> do
            {
                ma <- pushback st b;
                return $ fmap (fmap (pure . SumEditLeft . MkWholeEdit)) ma;
            };
            SumEditRight editb -> do
            {
                mstateedita <- floatingEditLensPutEdit lens st editb;
                return $ fmap (fmap (fmap SumEditRight)) mstateedita;
            };
        }
    };
}
