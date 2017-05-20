module Truth.Core.Edit.EditFunction where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit.Edit;


    -- | A EditLens is a lens without state
    ;
    data EditFunction edita editb = MkEditFunction
    {
        editGet :: ReadFunction (EditReader edita) (EditReader editb),
        editUpdate :: edita -> Readable (EditReader edita) [editb]
    };

    editUpdates :: EditFunction edita editb -> [edita] -> Readable (EditReader edita) [editb];
    editUpdates ef eas = fmap mconcat $ for eas (editUpdate ef);

    instance Category EditFunction where
    {
        id = MkEditFunction
        {
            editGet = readable,
            editUpdate = \edit -> return [edit]
        };
        bc . ab = MkEditFunction
        {
            editGet = composeReadFunction (editGet bc) (editGet ab),
            editUpdate = \editA -> do
            {
                editBs <- editUpdate ab editA;
                editCss <- for editBs $ \editB -> mapReadable (editGet ab) $ editUpdate bc editB;
                return $ mconcat editCss;
            }
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
        editUpdate = \ea -> return $ cleanEditUpdate ff ea
    };
}
