module Truth.Core.Types.SumWhole where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;
    import Truth.Core.Types.Sum;


    type SumWholeReaderEdit reader edit = SumEdit (WholeReaderEdit reader) edit;
    type SumWholeEdit edit = SumWholeReaderEdit (EditReader edit) edit;

    sumWholeCleanEditFunction :: (Edit edita,FullEdit editb) =>
     CleanEditFunction edita editb -> CleanEditFunction (SumWholeEdit edita) editb;
    sumWholeCleanEditFunction cef = MkCleanEditFunction
    {
        cleanEditGet = cleanEditGet cef,
        cleanEditUpdate = \editewa -> case editewa of
        {
            SumEditLeft (MkWholeEdit a) -> getReplaceEdits $ fromCleanReadFunction (cleanEditGet cef) a;
            SumEditRight edita -> cleanEditUpdate cef edita;
        }
    };

    sumWholeCleanEditLens :: (Functor m,Edit edita,FullEdit editb) =>
     CleanEditLens' m edita editb ->
     CleanEditLens' m (SumWholeEdit edita) editb;
    sumWholeCleanEditLens lens = MkCleanEditLens
    {
        cleanEditLensFunction = sumWholeCleanEditFunction (cleanEditLensFunction lens),
        cleanEditLensPutEdit = \editb -> fmap (fmap SumEditRight) (cleanEditLensPutEdit lens editb)
    };

    sumWholeFloatingEditFunction :: (Reader (EditReader edita), FullReader (EditReader editb)) =>
     FloatingEditFunction state edita editb ->
     FloatingEditFunction state (SumWholeEdit edita) (SumWholeEdit editb);
    sumWholeFloatingEditFunction fef = MkFloatingEditFunction
    {
        floatingEditInitial = floatingEditInitial fef,
        floatingEditGet = floatingEditGet fef,
        floatingEditUpdate = \pedita oldstate -> case pedita of
        {
            SumEditLeft (MkWholeEdit a) -> let
            {
                b = fromReadFunction (floatingEditGet fef oldstate) a
            } in return (oldstate,return $ SumEditLeft $ MkWholeEdit b); -- state unchanged, kind of dubious
            SumEditRight edita -> do
            {
                (newstate,meditb) <- floatingEditUpdate fef edita oldstate;
                return (newstate,fmap SumEditRight meditb);
            };
        }
    };

    sumWholeFloatingEditLens :: (Functor f,Reader (EditReader edita),FullReader (EditReader editb)) =>
     (state -> EditSubject editb -> Readable (EditReader edita) (f (state,EditSubject edita))) ->
     FloatingEditLens' f state edita editb ->
     FloatingEditLens' f state (SumWholeEdit edita) (SumWholeEdit editb);
    sumWholeFloatingEditLens pushback lens = MkFloatingEditLens
    {
        floatingEditLensFunction = sumWholeFloatingEditFunction (floatingEditLensFunction lens),
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
