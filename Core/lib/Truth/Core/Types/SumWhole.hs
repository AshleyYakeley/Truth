module Truth.Core.Types.SumWhole where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;
    import Truth.Core.Types.Sum;


    type SumWholeEdit edit = SumEdit (WholeEdit (EditReader edit)) edit;

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
        cleanEditLensPutEdit = \editb -> fmap SumEditRight (cleanEditLensPutEdit lens editb)
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
            } in (oldstate,return $ SumEditLeft $ MkWholeEdit b); -- state unchanged, kind of dubious
            SumEditRight edita -> let
            {
                (newstate,meditb) = floatingEditUpdate fef edita oldstate;
            } in (newstate,fmap SumEditRight meditb);
        }
    };

    sumWholeFloatingEditLens :: (Reader (EditReader edita),FullReader (EditReader editb)) =>
     (state -> EditSubject editb -> Readable (EditReader edita) (Maybe (state,EditSubject edita))) ->
     FloatingEditLens state edita editb ->
     FloatingEditLens state (SumWholeEdit edita) (SumWholeEdit editb);
    sumWholeFloatingEditLens pushback lens = MkFloatingEditLens
    {
        floatingEditLensFunction = sumWholeFloatingEditFunction (floatingEditLensFunction lens),
        floatingEditLensPutEdit = \state peditb -> case peditb of
        {
            SumEditLeft (MkWholeEdit b) -> do
            {
                ma <- pushback state b;
                return $ fmap (fmap (SumEditLeft . MkWholeEdit)) ma;
            };
            SumEditRight editb -> do
            {
                mstateedita <- floatingEditLensPutEdit lens state editb;
                return $ fmap (fmap SumEditRight) mstateedita;
            };
        }
    };
}
