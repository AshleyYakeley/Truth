module Truth.Core.Types.OneEdit where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.OneReader;


    newtype OneEdit (f :: * -> *) edit = MkOneEdit edit;

    instance Floating edit edit => Floating (OneEdit f edit) (OneEdit f edit) where
    {
        floatingUpdate (MkOneEdit e1) (MkOneEdit e2) = MkOneEdit $ floatingUpdate e1 e2;
    };

    instance (MonadOne f,Edit edit) => Edit (OneEdit f edit) where
    {
        type EditReader (OneEdit f edit) = OneReader f (EditReader edit);

        -- applyEdit :: OneEdit f edit -> ReadMap (OneReader f (EditReader edit)) (OneReader f (EditReader edit));
        applyEdit (MkOneEdit _edita) ReadHasOne = readable ReadHasOne;
        applyEdit (MkOneEdit edita) (ReadOne reader) = liftMaybeReadable (applyEdit edita reader);

        -- invertEdit :: OneEdit f edit -> Readable (OneReader f reader) (Maybe (OneEdit f edit));    -- "Nothing" means no change
        invertEdit (MkOneEdit edita) = do
        {
            fme <- liftMaybeReadable (invertEdit edita);
            return (case getMaybeOne fme of
            {
                Just edits -> fmap MkOneEdit edits;
                _ -> [];
            });
        };
    };

    oneLiftEditFunction :: forall f state edita editb. (MonadOne f) =>
        EditFunction state edita editb -> EditFunction state (OneEdit f edita) (OneEdit f editb);
    oneLiftEditFunction ff = MkEditFunction
    {
        editInitial = editInitial ff,
        editGet = \curstate -> liftMaybeReadFunction (editGet ff curstate),
        editUpdate = \(MkOneEdit edita) oldstate -> do
        {
            fr <- liftMaybeReadable $ editUpdate ff edita oldstate;
            return $ case retrieveOne fr of
            {
                SuccessResult (newstate,editBs) -> (newstate,fmap MkOneEdit editBs);
                FailureResult _fx -> (oldstate,[]);
            };
        }
    };
{-
    oneLiftEditLens :: forall c m f state edita editb. (ReadableConstraint c,Applicative m,MonadOne f) =>
        EditLens' c m state edita editb -> EditLens' c m state (OneEdit f edita) (OneEdit f editb);
    oneLiftEditLens lens = MkEditLens
    {
        editLensFunction = oneLiftEditFunction (editLensFunction lens),
        editLensPutEdit = \oldstate (MkOneEdit editb) -> do
        {
            fmeditas <- liftMaybeReadable $ editLensPutEdit lens oldstate editb;
            return $ case retrieveOne fmeditas of
            {
                SuccessResult meditas -> fmap (\(newstate,editas) -> (newstate,fmap MkOneEdit editas)) meditas;
                FailureResult _fx -> pure (oldstate,[MkOneEdit undefined]); -- any OneEdit edit will do
            };
        }
    };
-}
    oneLiftEditLens :: forall m f state edita editb. (Monad m,MonadOne f) =>
        (forall a. f a -> m a) ->
        EditLens' m state edita editb -> EditLens' m state (OneEdit f edita) (OneEdit f editb);
    oneLiftEditLens faffa lens = MkEditLens
    {
        editLensFunction = oneLiftEditFunction (editLensFunction lens),
        editLensPutEdit = \oldstate (MkOneEdit editb) -> do
        {
            fmeditas <- liftMaybeReadable $ editLensPutEdit lens oldstate editb;
            return $ do
            {
                meditas <- faffa fmeditas;
                (newstate,editas) <- meditas;
                return (newstate,fmap MkOneEdit editas);
            };
        }
    };

    oneLiftGeneralLens :: MonadOne f => GeneralLens edita editb -> GeneralLens (OneEdit f edita) (OneEdit f editb);
    oneLiftGeneralLens (MkCloseState lens) = MkCloseState $ oneLiftEditLens getMaybeOne lens;
}
