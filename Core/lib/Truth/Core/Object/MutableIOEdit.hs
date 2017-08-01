module Truth.Core.Object.MutableIOEdit where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.None;
    import Truth.Core.Object.MutableEdit;


    data MutableIOReader edit t where
    {
        ReadMutableIO :: MutableIOReader edit (MutableEdit IO edit);
    };

    instance Reader (EditReader edit) => Reader (MutableIOReader edit) where
    {
        type ReaderSubject (MutableIOReader edit) = EditSubject edit;
        readFrom subj ReadMutableIO = constantMutableEdit subj;
    };

    instance IOFullReader (EditReader edit) => GenFullReader MonadIO (MutableIOReader edit) where
    {
        genFromReader = do
        {
            muted <- readable ReadMutableIO;
            liftIO $ unReadable ioFromReader $ mutableRead muted;
        };
    };

    type MutableIOEdit edit = NoEdit (MutableIOReader edit);

    $(return []);
    instance HasTypeInfo MutableIOReader where
    {
        typeWitness = $(generateWitness [t|MutableIOReader|]);
        typeName _ = "MutableIOReader";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance Reader (EditReader edit) => Reader (MutableIOReader edit) where
            {
                type ReaderSubject (MutableIOReader edit) = EditSubject edit;
            };
            instance IOFullReader (EditReader edit) => GenFullReader MonadIO (MutableIOReader edit);
        |]);
    };

    mutableIOEditLens :: forall m edit. Applicative m => IOFloatingEditLens' m () (MutableIOEdit edit) edit;
    mutableIOEditLens = let
    {
        floatingEditInitial = ();

        floatingEditGet :: () -> IOReadFunction (MutableIOReader edit) (EditReader edit);
        floatingEditGet () reader = do
        {
            muted <- readable ReadMutableIO;
            liftIO $ mutableRead muted reader;
        };

        floatingEditUpdate :: MutableIOEdit edit -> () -> IOReadable (MutableIOReader edit) ((),[edit]);
        floatingEditUpdate edit = never edit;

        floatingEditLensFunction = MkFloatingEditFunction{..};

        floatingEditLensPutEdit :: () -> edit -> IOReadable (MutableIOReader edit) (m ((),[MutableIOEdit edit]));
        floatingEditLensPutEdit () edit = do
        {
            muted <- readable ReadMutableIO;
            liftIO $ do
            {
                maction <- mutableEdit muted [edit];
                case maction of
                {
                    Just action -> action;
                    Nothing -> fail "mutableIOEditLens: failed";
                };
            };
            return $ pure ((),[]);
        };
    } in MkFloatingEditLens{..};

    mutableIOLiftEditLens :: forall c c' f f' edita editb. (ReadableConstraint c,MonadOne f,Edit edita,c IO) => GenFloatingEditLens' c f () edita editb -> GenFloatingEditLens' c' f' () (MutableIOEdit edita) (MutableIOEdit editb);
    mutableIOLiftEditLens lens = let
    {
        floatingEditInitial = ();
        floatingEditGet :: () -> GenReadFunction c' (MutableIOReader edita) (MutableIOReader editb);
        floatingEditGet () ReadMutableIO = do
        {
            muted <- readable ReadMutableIO;
            return $ fixedMapMutableEdit lens muted;
        };
        floatingEditUpdate edita = never edita;
        floatingEditLensFunction = MkFloatingEditFunction{..};
        floatingEditLensPutEdit () editb = never editb;
    } in MkFloatingEditLens{..};
}
