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

    instance FullReader (EditReader edit) => FullReader (MutableIOReader edit) where
    {
        fromReader = do
        {
            muted <- readable ReadMutableIO;
            liftIO $ unReadable fromReader $ mutableRead muted;
        };
    };

    type MutableIOEdit edit = NoEdit (MutableIOReader edit);

    mutableIOEditLens :: forall m edit. Applicative m => EditLens' m () (MutableIOEdit edit) edit;
    mutableIOEditLens = let
    {
        editInitial = ();

        editGet :: () -> ReadFunction (MutableIOReader edit) (EditReader edit);
        editGet () reader = do
        {
            muted <- readable ReadMutableIO;
            liftIO $ mutableRead muted reader;
        };

        editUpdate :: MutableIOEdit edit -> () -> Readable (MutableIOReader edit) ((),[edit]);
        editUpdate edit = never edit;

        editLensFunction = MkEditFunction{..};

        editLensPutEdit :: () -> edit -> Readable (MutableIOReader edit) (m ((),[MutableIOEdit edit]));
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

    mutableIOLiftEditLens :: forall f f' edita editb. (MonadOne f,Edit edita) => EditLens' f () edita editb -> EditLens' f' () (MutableIOEdit edita) (MutableIOEdit editb);
    mutableIOLiftEditLens lens = let
    {
        editInitial = ();
        editGet :: () -> ReadFunction (MutableIOReader edita) (MutableIOReader editb);
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
