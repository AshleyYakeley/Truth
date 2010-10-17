module Truth.Edit.EditFunction where
{
    import Truth.Edit.JustEdit;
    import Truth.Edit.Either();
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    -- | A EditLens is a lens without state
    ;
    data EditFunction edita editb = MkEditFunction
    {
        editGet :: Subject edita -> Subject editb,
        editUpdate :: edita -> ConstFunction (Subject edita) (Maybe editb)
    };

    instance Category EditFunction where
    {
        id = MkEditFunction
        {
            editGet = id,
            editUpdate = \edit -> pure (Just edit)
        };
        bc . ab = MkEditFunction
        {
            editGet = (editGet bc) . (editGet ab),
            editUpdate = \edita -> do
            {
                meb <- editUpdate ab edita;
                case meb of
                {
                    Just editb -> cofmap1CF (editGet ab) (editUpdate bc editb);
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
                fmeditb <- cfmap (editUpdate lens edita);
                return (case retrieveOne fmeditb of
                {
                    SuccessResult (Just editb) -> Just (MkJustEdit editb);
                    _ -> Nothing;
                });
            },
            editGet = cfmap (editGet lens)
        };
    };

    data CleanEditFunction edita editb = MkCleanEditFunction
    {
        cleanEditGet :: Subject edita -> Subject editb,
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
            cleanEditGet = (cleanEditGet bc) . (cleanEditGet ab),
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
        editGet = cleanEditGet ff,
        editUpdate = \edit -> pure (cleanEditUpdate ff edit)
    };

    editFunction :: (a -> b) -> EditFunction (WholeEdit a) (WholeEdit b);
    editFunction ab = MkEditFunction
    {
        editGet = ab,
        editUpdate = \(MkWholeEdit a) -> pure (Just (MkWholeEdit (ab a)))
    };

    simpleConvertEditFunction :: (Edit edita,FullEdit editb) => (Subject edita -> Subject editb) -> EditFunction edita editb;
    simpleConvertEditFunction ab = MkEditFunction
    {
        editGet = ab,
        editUpdate = \edit -> fmap (Just . replaceEdit . ab) (applyEdit edit)
    };

    convertEditFunction :: (Edit edita,FullEdit editb,Subject edita ~ Subject editb) => EditFunction edita editb;
    convertEditFunction = MkEditFunction
    {
        editGet = id,
        editUpdate = \edit -> fmap (Just . replaceEdit) (applyEdit edit)
    };

    eitherEditFunction :: (Subject edit ~ Subject edit') => CleanEditFunction edit (Either edit' edit);
    eitherEditFunction = MkCleanEditFunction
    {
        cleanEditGet = id,
        cleanEditUpdate = \edit -> Just (Right edit)
    };

    withWholeEditFunction :: (FullEdit editb) =>
     CleanEditFunction edita editb -> CleanEditFunction (Either (WholeEdit (Subject edita)) edita) editb;
    withWholeEditFunction cef = MkCleanEditFunction
    {
        cleanEditGet = cleanEditGet cef,
        cleanEditUpdate = \editewa -> case editewa of
        {
            Left (MkWholeEdit a) -> Just (replaceEdit (cleanEditGet cef a));
            Right edita -> cleanEditUpdate cef edita;
        }
    };
}
