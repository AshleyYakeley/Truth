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

    instance (MonadOne f) => CatFunctor EditFunction (OneEdit f) where
    {
        cfmap lens = MkEditFunction
        {
            editUpdate = \(MkOneEdit edita) -> do
            {
                feditBs <- liftMaybeReadable $ editUpdate lens edita;
                return $ case retrieveOne feditBs of
                {
                    SuccessResult editBs -> fmap MkOneEdit editBs;
                    FailureResult _fx -> [];
                };
            },
            editGet = liftMaybeReadFunction (editGet lens)
        };
    };

    instance (MonadOne f,Applicative m) => CatFunctor (EditLens' m) (OneEdit f) where
    {
        cfmap lens = MkEditLens
        {
            editLensFunction = cfmap (editLensFunction lens),
            editLensPutEdit = \(MkOneEdit editb) -> do
            {
                fmeditas <- liftMaybeReadable $ editLensPutEdit lens editb;
                return $ case retrieveOne fmeditas of
                {
                    SuccessResult meditas -> fmap (fmap MkOneEdit) meditas;
                    FailureResult _fx -> pure [MkOneEdit undefined]; -- any OneEdit edit will do
                };
            }
        };
    };

    $(return []);
    instance HasTypeInfo OneEdit where
    {
        typeWitness = $(generateWitness [t|OneEdit|]);
        typeName _ = "OneEdit";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance (MonadOne f,Edit edit) => Edit (OneEdit f edit) where
            {
                type EditReader (OneEdit f edit) = OneReader f (EditReader edit);
            };
        |]);
    };

    oneFloatingEditFunction :: forall f state edita editb. (MonadOne f,Edit edita,Edit editb) =>
     FloatingEditFunction state edita editb -> FloatingEditFunction state (OneEdit f edita) (OneEdit f editb);
    oneFloatingEditFunction fef = MkFloatingEditFunction
    {
        floatingEditInitial = floatingEditInitial fef,
        floatingEditGet = \st -> liftMaybeReadFunction (floatingEditGet fef st),
        floatingEditUpdate = \(MkOneEdit edita) oldstate -> do
        {
            fstuff <- liftMaybeReadable $ floatingEditUpdate fef edita oldstate;
            return $ case retrieveOne fstuff of
            {
                SuccessResult (newstate,editBs) -> (newstate,fmap MkOneEdit editBs);
                FailureResult _fx -> (oldstate,[]);
            }
        }
    };

    oneFloatingEditLens :: forall ff f state edita editb. (Monad ff,MonadOne f,Edit edita,Edit editb) =>
     (forall a. f a -> ff a) ->
     FloatingEditLens' ff state edita editb -> FloatingEditLens' ff state (OneEdit f edita) (OneEdit f editb);
    oneFloatingEditLens faffa lens = MkFloatingEditLens
    {
        floatingEditLensFunction = oneFloatingEditFunction (floatingEditLensFunction lens),
        floatingEditLensPutEdit = \oldstate (MkOneEdit pushb) -> do
        {
            ffpusha <- liftMaybeReadable (floatingEditLensPutEdit lens oldstate pushb);
            return $ do
            {
                fpusha <- faffa ffpusha;
                (newstate,editas) <- fpusha;
                return (newstate,fmap MkOneEdit editas);
            };
        }
    };
}
