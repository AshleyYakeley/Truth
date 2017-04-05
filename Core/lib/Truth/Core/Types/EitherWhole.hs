module Truth.Core.Types.EitherWhole where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;
    import Truth.Core.Types.Either;


    type EitherWholeEdit edit = EitherEdit (WholeEdit (EditReader edit)) edit;

    eitherWholeCleanEditFunction :: (Edit edita,FullEdit editb) =>
     CleanEditFunction edita editb -> CleanEditFunction (EitherWholeEdit edita) editb;
    eitherWholeCleanEditFunction cef = MkCleanEditFunction
    {
        cleanEditGet = cleanEditGet cef,
        cleanEditUpdate = \editewa -> case editewa of
        {
            LeftEdit (MkWholeEdit a) -> fromReadable replaceEdit $ fromCleanReadFunction (cleanEditGet cef) a;
            RightEdit edita -> cleanEditUpdate cef edita;
        }
    };

    eitherWholeCleanEditLens :: (Functor m,Edit edita,FullEdit editb) =>
     CleanEditLens' m edita editb ->
     CleanEditLens' m (EitherWholeEdit edita) editb;
    eitherWholeCleanEditLens lens = MkCleanEditLens
    {
        cleanEditLensFunction = eitherWholeCleanEditFunction (cleanEditLensFunction lens),
        cleanEditLensPutEdit = \editb -> fmap RightEdit (cleanEditLensPutEdit lens editb)
    };

    eitherWholeFloatingEditFunction :: (Reader (EditReader edita), FullReader (EditReader editb)) =>
     FloatingEditFunction state edita editb ->
     FloatingEditFunction state (EitherWholeEdit edita) (EitherWholeEdit editb);
    eitherWholeFloatingEditFunction fef = MkFloatingEditFunction
    {
        floatingEditInitial = floatingEditInitial fef,
        floatingEditGet = floatingEditGet fef,
        floatingEditUpdate = \pedita oldstate -> case pedita of
        {
            LeftEdit (MkWholeEdit a) -> let
            {
                b = fromReadFunction (floatingEditGet fef oldstate) a
            } in (oldstate,return $ LeftEdit $ MkWholeEdit b); -- state unchanged, kind of dubious
            RightEdit edita -> let
            {
                (newstate,meditb) = floatingEditUpdate fef edita oldstate;
            } in (newstate,fmap RightEdit meditb);
        }
    };

    eitherWholeFloatingEditLens :: (Reader (EditReader edita),FullReader (EditReader editb)) =>
     (state -> EditSubject editb -> Readable (EditReader edita) (Maybe (state,EditSubject edita))) ->
     FloatingEditLens state edita editb ->
     FloatingEditLens state (EitherWholeEdit edita) (EitherWholeEdit editb);
    eitherWholeFloatingEditLens pushback lens = MkFloatingEditLens
    {
        floatingEditLensFunction = eitherWholeFloatingEditFunction (floatingEditLensFunction lens),
        floatingEditLensPutEdit = \state peditb -> case peditb of
        {
            LeftEdit (MkWholeEdit b) -> do
            {
                ma <- pushback state b;
                return $ fmap (fmap (LeftEdit . MkWholeEdit)) ma;
            };
            RightEdit editb -> do
            {
                mstateedita <- floatingEditLensPutEdit lens state editb;
                return $ fmap (fmap RightEdit) mstateedita;
            };
        }
    };
}
