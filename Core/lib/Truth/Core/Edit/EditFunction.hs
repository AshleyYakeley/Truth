module Truth.Core.Edit.EditFunction where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit.Edit;


    -- | A EditLens is a lens without state
    ;
    data GenEditFunction c edita editb = MkEditFunction
    {
        editGet :: GenReadFunction c (EditReader edita) (EditReader editb),
        editUpdate :: edita -> GenReadable c (EditReader edita) [editb]
    };

    type EditFunction = GenEditFunction Monad;
    type IOEditFunction = GenEditFunction MonadIO;

    editFunctionToGen :: EditFunction edita editb -> GenEditFunction c edita editb;
    editFunctionToGen (MkEditFunction g u) = MkEditFunction (readFunctionToGen g) (\ea -> readableToGen $ u ea);

    editUpdates :: EditFunction edita editb -> [edita] -> Readable (EditReader edita) [editb];
    editUpdates ef eas = fmap mconcat $ for eas (editUpdate ef);

    instance ReadableConstraint c => Category (GenEditFunction c) where
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
                editCss <- for editBs $ \editB -> mapGenReadable (editGet ab) $ editUpdate bc editB;
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

    convertEditFunction :: (EditSubject edita ~ EditSubject editb,Edit edita,FullReader (EditReader edita),FullEdit editb) => EditFunction edita editb;
    convertEditFunction = let
    {
        editGet = convertReadFunction;
        editUpdate edita = do
        {
            newsubject <- mapReadable (applyEdit edita) fromReader;
            return $ getReplaceEdits newsubject;
        };
    } in MkEditFunction{..};
}
