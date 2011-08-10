module Truth.Edit.EditFunction where
{
    import Truth.Edit.JustEdit;
    import Truth.Edit.Either();
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Edit;
    import Truth.Edit.Either;
    import Truth.Edit.ReadFunction;
    import Truth.Edit.Read;
    import Truth.Edit.Import;

    -- | A EditLens is a lens without state
    ;
    data EditFunction edita editb = MkEditFunction
    {
        editGet :: ReadFunction (EditReader edita) (EditReader editb),
        editUpdate :: edita -> Readable (EditReader edita) (Maybe editb)
    };

    instance Category EditFunction where
    {
        id = MkEditFunction
        {
            editGet = readable,
            editUpdate = \edit -> pure (Just edit)
        };
        bc . ab = MkEditFunction
        {
            editGet = composeReadFunction (editGet bc) (editGet ab),
            editUpdate = \edita -> do
            {
                meb <- editUpdate ab edita;
                case meb of
                {
                    Just editb -> mapReadable (editGet ab) (editUpdate bc editb);
                    _ -> return Nothing;
                };
            }
        };
    };

    instance (FunctorOne f) => CatFunctor EditFunction (JustEdit f) where
    {
        cfmap lens = MkEditFunction
        {
            editUpdate = \(MkJustEdit edita) ->  do
            {
                fmeditb <- liftJustReadable (editUpdate lens edita);
                return (case retrieveOne fmeditb of
                {
                    SuccessResult (Just editb) -> Just (MkJustEdit editb);
                    _ -> Nothing;
                });
            },
            editGet = liftJustReadFunction (editGet lens)
        };
    };

    data CleanEditFunction edita editb = MkCleanEditFunction
    {
        cleanEditGet :: CleanReadFunction (EditReader edita) (EditReader editb),
        cleanEditUpdate :: edita -> Maybe editb
    };

    --type CleanEditLens = CleanEditLens' Maybe;

    instance Category CleanEditFunction where
    {
        id = MkCleanEditFunction
        {
            cleanEditGet = id,
            cleanEditUpdate = Just
        };
        bc . ab = MkCleanEditFunction
        {
            cleanEditGet = (cleanEditGet ab) . (cleanEditGet bc),
            cleanEditUpdate = \edita -> do
            {
                editb <- cleanEditUpdate ab edita;
                cleanEditUpdate bc editb;
            }
        };
    };

    cleanEditFunction :: CleanEditFunction edita editb -> EditFunction edita editb;
    cleanEditFunction ff = MkEditFunction
    {
        editGet = cleanReadFunction (cleanEditGet ff),
        editUpdate = \edit -> pure (cleanEditUpdate ff edit)
    };

    editFunction :: (a -> b) -> EditFunction (WholeEdit (WholeReader a)) (WholeEdit (WholeReader b));
    editFunction ab = MkEditFunction
    {
        editGet = simpleReadFunction ab,
        editUpdate = \(MkWholeEdit a) -> pure (Just (MkWholeEdit (ab a)))
    };

    simpleConvertEditFunction ::
     (Edit edita,FullEdit editb) => (ReadFunction (EditReader edita) (EditReader editb)) -> EditFunction edita editb;
    simpleConvertEditFunction rfrarb = MkEditFunction
    {
        editGet = rfrarb,

        editUpdate = \edit -> do
        {
            b <- mapReadable (applyEdit edit) (mapReadable rfrarb fromReader);
            return (Just (replaceEdit b));
        }
    };

    convertEditFunction :: (FullReader (EditReader edita),Edit edita,FullEdit editb,EditSubject edita ~ EditSubject editb) => EditFunction edita editb;
    convertEditFunction = simpleConvertEditFunction convertReadFunction;

    eitherEditFunction :: (EditReader edit ~ EditReader edit') => CleanEditFunction edit (EitherEdit edit' edit);
    eitherEditFunction = MkCleanEditFunction
    {
        cleanEditGet = id,
        cleanEditUpdate = \edit -> Just (RightEdit edit)
    };

    withWholeEditFunction :: (Edit edita,FullEdit editb) =>
     CleanEditFunction edita editb -> CleanEditFunction (EitherEdit (WholeEdit (EditReader edita)) edita) editb;
    withWholeEditFunction cef = MkCleanEditFunction
    {
        cleanEditGet = cleanEditGet cef,
        cleanEditUpdate = \editewa -> case editewa of
        {
            LeftEdit (MkWholeEdit a) -> Just (replaceEdit (fromCleanReadFunction (cleanEditGet cef) a));
            RightEdit edita -> cleanEditUpdate cef edita;
        }
    };
}
