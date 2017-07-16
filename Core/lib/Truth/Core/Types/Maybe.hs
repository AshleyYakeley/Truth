module Truth.Core.Types.Maybe where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.OneReader;


    data MaybeEdit edit =
        CreateMaybeEdit| -- create as newValue, or do nothing if it already exists
        DeleteMaybeEdit| -- remove
        JustMaybeEdit edit; -- change existing value if it exists, otherwise do nothing

    instance Floating edit edit => Floating (MaybeEdit edit) (MaybeEdit edit) where
    {
        floatingUpdate (JustMaybeEdit e1) (JustMaybeEdit e2) = JustMaybeEdit $ floatingUpdate e1 e2;
        floatingUpdate _ t = t;
    };

    instance (IOFullEdit edit,HasNewValue (EditSubject edit)) => Edit (MaybeEdit edit) where
    {
        type EditReader (MaybeEdit edit) = OneReader Maybe (EditReader edit);

        applyEdit CreateMaybeEdit ReadHasOne = return $ Just ();
        applyEdit CreateMaybeEdit (ReadOne reader) = return $ Just $ readFrom newValue reader;
        applyEdit DeleteMaybeEdit reader = return $ readFrom Nothing reader;
        applyEdit (JustMaybeEdit edita) (ReadOne reader) = liftMaybeReadable (applyEdit edita reader);
        applyEdit (JustMaybeEdit _edita) reader = readable reader;

        -- invertEdit :: MaybeEdit edit -> Readable (OneReader Maybe reader) [OneEdit Maybe edit];    -- "Nothing" means no change
        invertEdit CreateMaybeEdit = do
        {
            me <- readable ReadHasOne;
            case me of
            {
                Just () -> return [];
                Nothing -> return [DeleteMaybeEdit]; -- deleted
            };
        };
        invertEdit DeleteMaybeEdit = do
        {
            me <- readable ReadHasOne;
            case me of
            {
                Just () -> do
                {
                    medits <- mapReadableF (readable . ReadOne) $ ioWriterToReadable ioReplaceEdit;
                    case medits of
                    {
                        Nothing -> return []; -- shouldn't happen
                        Just edits -> return $ CreateMaybeEdit : (fmap JustMaybeEdit edits);
                    }
                };
                Nothing -> return []; -- already deleted
            };
        };
        invertEdit (JustMaybeEdit edita) = do
        {
            me <- liftMaybeReadable (invertEdit edita);
            return $ case me of
            {
                Just edits -> fmap JustMaybeEdit edits;
                Nothing -> [];
            };
        };
    };

    instance (IOFullEdit edit,HasNewValue (EditSubject edit)) => IOFullEdit (MaybeEdit edit) where
    {
        ioReplaceEdit = do
        {
            me <- readable ReadHasOne;
            case me of
            {
                Just () -> do
                {
                    wrWrite CreateMaybeEdit;
                    _ <- reWriterReadable JustMaybeEdit $ mapReadableF (readable . ReadOne) ioReplaceEdit;
                    return ();
                };
                Nothing -> wrWrite DeleteMaybeEdit; -- deleted
            };
        };
    };

    instance (FullEdit edit,HasNewValue (EditSubject edit)) => FullEdit (MaybeEdit edit) where
    {
        replaceEdit = do
        {
            me <- readable ReadHasOne;
            case me of
            {
                Just () -> do
                {
                    wrWrite CreateMaybeEdit;
                    _ <- reWriterReadable JustMaybeEdit $ mapReadableF (readable . ReadOne) replaceEdit;
                    return ();
                };
                Nothing -> wrWrite DeleteMaybeEdit; -- deleted
            };
        };
    };

    $(return []);
    instance HasInfo MaybeEdit where
    {
        info = mkSimpleInfo $(ionamedwitness[t|MaybeEdit|]) [$(declInfo [d|
            instance (FullEdit edit,HasNewValue (EditSubject edit)) => Edit (MaybeEdit edit) where
            {
                type EditReader (MaybeEdit edit) = OneReader Maybe (EditReader edit);
            };
            instance (FullEdit edit,HasNewValue (EditSubject edit)) => FullEdit (MaybeEdit edit);
        |])];
    };
}
