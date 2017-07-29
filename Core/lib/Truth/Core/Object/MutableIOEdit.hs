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

    mutableIOEditLens :: forall edit. IOEditLens' Identity (MutableIOEdit edit) edit;
    mutableIOEditLens = let
    {
        editGet :: IOReadFunction (MutableIOReader edit) (EditReader edit);
        editGet reader = do
        {
            muted <- readable ReadMutableIO;
            liftIO $ mutableRead muted reader;
        };

        editUpdate :: MutableIOEdit edit -> IOReadable (MutableIOReader edit) [edit];
        editUpdate edit = never edit;

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: edit -> IOReadable (MutableIOReader edit) (Identity [MutableIOEdit edit]);
        editLensPutEdit edit = do
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
            return $ Identity [];
        };
    } in MkEditLens{..};

    liftMutableIOEditLens :: forall f f' edita editb. (MonadOne f,Edit edita) => EditLens' f edita editb -> EditLens' f' (MutableIOEdit edita) (MutableIOEdit editb);
    liftMutableIOEditLens lens = let
    {
        editGet :: ReadFunction (MutableIOReader edita) (MutableIOReader editb);
        editGet ReadMutableIO = do
        {
            muted <- readable ReadMutableIO;
            return $ fixedMapMutableEdit lens muted;
        };
        editUpdate edita = never edita;
        editLensFunction = MkEditFunction{..};
        editLensPutEdit editb = never editb;
    } in MkEditLens{..};
}
