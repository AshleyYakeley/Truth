module Truth.Edit.MaybeEdit where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.MonadOneReader;
    import Truth.Edit.Edit;


    data MaybeEdit edit =
        CreateMaybeEdit| -- create as newValue, or do nothing if it already exists
        DeleteMaybeEdit| -- remove
        JustMaybeEdit edit; -- change existing value if it exists, otherwise do nothing

    instance Floating (MaybeEdit edit) (MaybeEdit edit);

    instance (FullEdit edit,HasNewValue (EditSubject edit)) => Edit (MaybeEdit edit) where
    {
        type EditReader (MaybeEdit edit) = MonadOneReader Maybe (EditReader edit);

        applyEdit CreateMaybeEdit ReadHasOne = return $ Just ();
        applyEdit CreateMaybeEdit (ReadOne reader) = return $ Just $ readFrom newValue reader;
        applyEdit DeleteMaybeEdit reader = return $ readFrom Nothing reader;
        applyEdit (JustMaybeEdit edita) (ReadOne reader) = liftMaybeReadable (applyEdit edita reader);
        applyEdit (JustMaybeEdit _edita) reader = readable reader;

        -- invertEdit :: MaybeEdit edit -> Readable (MonadOneReader Maybe reader) [JustEdit Maybe edit];    -- "Nothing" means no change
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
                    medits <- mapReadableF (readable . ReadOne) replaceEdit;
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

    instance (FullEdit edit,HasNewValue (EditSubject edit)) => FullEdit (MaybeEdit edit) where
    {
        replaceEdit = do
        {
            me <- readable ReadHasOne;
            case me of
            {
                Just () -> do
                {
                    medits <- mapReadableF (readable . ReadOne) replaceEdit;
                    case medits of
                    {
                        Nothing -> return [DeleteMaybeEdit]; -- shouldn't happen
                        Just edits -> return $ CreateMaybeEdit : (fmap JustMaybeEdit edits);
                    }
                };
                Nothing -> return [DeleteMaybeEdit]; -- deleted
            };
        };
    };

    instance HasInfo MaybeEdit where
    {
        info = mkSimpleInfo $(iowitness[t|MaybeEdit|])
        [
            -- instance (Edit edit,HasNewValue (EditSubject edit)) => Edit (MaybeEdit edit)
            MkKnowledge $ \knowledge ejfe -> do
            {
                MkSplitInfo edit me <- matchInfo ejfe;
                ReflH <- testHetEquality (info @Edit) edit;
                MkSplitInfo m editVar <- matchInfo me;
                ReflH <- testHetEquality (info @MaybeEdit) m;
                ConstraintFact <- ask knowledge $ applyInfo (info @FullEdit) editVar;
                ValueFact (MkEditReaderInfo readerVar) <- ask knowledge $ applyInfo (info @EditReaderInfo) editVar;
                ValueFact (MkReaderSubjectInfo subjVar) <- ask knowledge $ applyInfo (info @ReaderSubjectInfo) readerVar;
                ConstraintFact <- ask knowledge $ applyInfo (info @HasNewValue) subjVar;
                return ConstraintFact;
            }
        ];
    };
}
