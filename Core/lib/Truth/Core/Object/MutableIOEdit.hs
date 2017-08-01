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

    instance IOFullReader (EditReader edit) => FullReader MonadIO (MutableIOReader edit) where
    {
        fromReader = do
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
            instance IOFullReader (EditReader edit) => FullReader MonadIO (MutableIOReader edit);
        |]);
    };

    mutableIOEditLens :: forall m edit. Applicative m => IOEditLens' m () (MutableIOEdit edit) edit;
    mutableIOEditLens = let
    {
        editInitial = ();

        editGet :: () -> IOReadFunction (MutableIOReader edit) (EditReader edit);
        editGet () reader = do
        {
            muted <- readable ReadMutableIO;
            liftIO $ mutableRead muted reader;
        };

        editUpdate :: MutableIOEdit edit -> () -> IOReadable (MutableIOReader edit) ((),[edit]);
        editUpdate edit = never edit;

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> edit -> IOReadable (MutableIOReader edit) (m ((),[MutableIOEdit edit]));
        editLensPutEdit () edit = do
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
    } in MkEditLens{..};

    mutableIOLiftEditLens :: forall c c' f f' edita editb. (ReadableConstraint c,MonadOne f,Edit edita,c IO) => EditLens' c f () edita editb -> EditLens' c' f' () (MutableIOEdit edita) (MutableIOEdit editb);
    mutableIOLiftEditLens lens = let
    {
        editInitial = ();
        editGet :: () -> ReadFunction c' (MutableIOReader edita) (MutableIOReader editb);
        editGet () ReadMutableIO = do
        {
            muted <- readable ReadMutableIO;
            return $ fixedMapMutableEdit lens muted;
        };
        editUpdate edita = never edita;
        editLensFunction = MkEditFunction{..};
        editLensPutEdit () editb = never editb;
    } in MkEditLens{..};
}
