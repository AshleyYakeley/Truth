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
}
