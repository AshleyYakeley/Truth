module Truth.Edit.EditFunction where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Either;
    import Truth.Edit.Edit;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.MonadOneReader;
    import Truth.Edit.JustEdit;


    -- | A EditLens is a lens without state
    ;
    data EditFunction edita editb = MkEditFunction
    {
        editGet :: ReadFunction (EditReader edita) (EditReader editb),
        editUpdate :: edita -> [editb]
    };

    editUpdates :: EditFunction edita editb -> [edita] -> [editb];
    editUpdates ef eas = mconcat $ fmap (editUpdate ef) eas;

    instance Category EditFunction where
    {
        id = MkEditFunction
        {
            editGet = readable,
            editUpdate = \edit -> return edit
        };
        bc . ab = MkEditFunction
        {
            editGet = composeReadFunction (editGet bc) (editGet ab),
            editUpdate = \edita -> do
            {
                editb <- editUpdate ab edita;
                editUpdate bc editb;
            }
        };
    };

    instance (MonadOne f) => CatFunctor EditFunction (JustEdit f) where
    {
        cfmap lens = MkEditFunction
        {
            editUpdate = \(MkJustEdit edita) -> do
            {
                editb <- editUpdate lens edita;
                return $ MkJustEdit editb;
            },
            editGet = liftMaybeReadFunction (editGet lens)
        };
    };

    data CleanEditFunction edita editb = MkCleanEditFunction
    {
        cleanEditGet :: CleanReadFunction (EditReader edita) (EditReader editb),
        cleanEditUpdate :: edita -> [editb]
    };

    --type CleanEditLens = CleanEditLens' Maybe;

    instance Category CleanEditFunction where
    {
        id = MkCleanEditFunction
        {
            cleanEditGet = id,
            cleanEditUpdate = return
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
        editUpdate = cleanEditUpdate ff
    };

    editFunction :: (a -> b) -> EditFunction (WholeEdit (WholeReader a)) (WholeEdit (WholeReader b));
    editFunction ab = MkEditFunction
    {
        editGet = simpleReadFunction ab,
        editUpdate = \(MkWholeEdit a) -> return $ MkWholeEdit $ ab a
    };
{-
    simpleConvertEditFunction :: forall edita editb.
     (Edit edita,FullEdit editb) => ReadFunction (EditReader edita) (EditReader editb) -> EditFunction edita editb;
    simpleConvertEditFunction rfrarb = let
    {
        editGet :: ReadFunction (EditReader edita) (EditReader editb);
        editGet = rfrarb;

        editUpdate :: edita -> Maybe editb;
        editUpdate edit = let
        {
            olda = undefined;
            newa = fromReadFunction (applyEdit edit) olda;
            b = fromReadFunction rfrarb newa;
        } in (Just $ replaceEdit b)
    } in MkEditFunction{..};

    convertEditFunction :: (FullReader (EditReader edita),Edit edita,FullEdit editb,EditSubject edita ~ EditSubject editb) => EditFunction edita editb;
    convertEditFunction = simpleConvertEditFunction convertReadFunction;
-}
    eitherEditFunction :: (EditReader edit ~ EditReader edit') => CleanEditFunction edit (EitherEdit edit' edit);
    eitherEditFunction = MkCleanEditFunction
    {
        cleanEditGet = id,
        cleanEditUpdate = \edit -> return (RightEdit edit)
    };

    withWholeEditFunction :: (Edit edita,FullEdit editb) =>
     CleanEditFunction edita editb -> CleanEditFunction (EitherEdit (WholeEdit (EditReader edita)) edita) editb;
    withWholeEditFunction cef = MkCleanEditFunction
    {
        cleanEditGet = cleanEditGet cef,
        cleanEditUpdate = \editewa -> case editewa of
        {
            LeftEdit (MkWholeEdit a) -> fromReadable replaceEdit $ fromCleanReadFunction (cleanEditGet cef) a;
            RightEdit edita -> cleanEditUpdate cef edita;
        }
    };
}
