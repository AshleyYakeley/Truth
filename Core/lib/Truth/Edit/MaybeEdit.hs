module Truth.Edit.MaybeEdit where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Edit;
    import Truth.Edit.JustEdit;


    data MaybeEdit edit =
        CreateMaybeEdit| -- replace with newValue, even if it already exists
        DeleteMaybeEdit| -- remove
        JustMaybeEdit edit; -- change existing value if it exists, otherwise do nothing

    instance Floating (MaybeEdit edit) (MaybeEdit edit);

    instance (FullEdit edit,HasNewValue (EditSubject edit)) => Edit (MaybeEdit edit) where
    {
        type EditReader (MaybeEdit edit) = JustReader Maybe (EditReader edit);

        applyEdit CreateMaybeEdit reader = return $ readFrom (Just newValue) reader;
        applyEdit DeleteMaybeEdit reader = return $ readFrom Nothing reader;
        applyEdit (JustMaybeEdit edita) (ReadWholeJust reader) = liftJustReadable (applyEdit edita reader);
        applyEdit (JustMaybeEdit _edita) reader = readable reader;

        -- invertEdit :: MaybeEdit edit -> Readable (JustReader Maybe reader) [JustEdit Maybe edit];    -- "Nothing" means no change
        invertEdit CreateMaybeEdit = do
        {
            me <- readable ReadIsJust;
            case me of
            {
                Nothing -> do
                {
                    medits <- mapReadableF (readable . ReadWholeJust) replaceEdit;
                    case medits of
                    {
                        Nothing -> return [DeleteMaybeEdit]; -- shouldn't happen
                        Just edits -> return $ fmap JustMaybeEdit edits;
                    }
                };
                Just _ -> return [DeleteMaybeEdit]; -- deleted
            };
        };
        invertEdit DeleteMaybeEdit = do
        {
            me <- readable ReadIsJust;
            case me of
            {
                Nothing -> do
                {
                    medits <- mapReadableF (readable . ReadWholeJust) replaceEdit;
                    case medits of
                    {
                        Nothing -> return []; -- shouldn't happen
                        Just edits -> return $ CreateMaybeEdit : (fmap JustMaybeEdit edits);
                    }
                };
                Just _ -> return []; -- already deleted
            };
        };
        invertEdit (JustMaybeEdit edita) = do
        {
            me <- liftJustReadable (invertEdit edita);
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
            me <- readable ReadIsJust;
            case me of
            {
                Nothing -> do
                {
                    medits <- mapReadableF (readable . ReadWholeJust) replaceEdit;
                    case medits of
                    {
                        Nothing -> return [DeleteMaybeEdit]; -- shouldn't happen
                        Just edits -> return $ CreateMaybeEdit : (fmap JustMaybeEdit edits);
                    }
                };
                Just _ -> return [DeleteMaybeEdit]; -- deleted
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
