module Truth.Core.Object.ObjectEdit where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.None;
    import Truth.Core.Object.MutableEdit;
    import Truth.Core.Object.Object;


    data ObjectReader edit t where
    {
        ReadObject :: ObjectReader edit (Object edit);
    };

    instance Reader (EditReader edit) => Reader (ObjectReader edit) where
    {
        type ReaderSubject (ObjectReader edit) = EditSubject edit;
        readFrom subj ReadObject = nonlockingObject $ constantMutableEdit subj;
    };

    instance IOFullReader (EditReader edit) => IOFullReader (ObjectReader edit) where
    {
        ioFromReader = do
        {
            MkObject object <- readable ReadObject;
            liftIO $ object $ \muted -> unReadable ioFromReader $ mutableRead muted;
        };
    };

    $(return []);
    instance HasTypeInfo ObjectReader where
    {
        typeWitness = $(generateWitness [t|ObjectReader|]);
        typeName _ = "ObjectReader";
        typeKnowledge _ = $(declInfo [d|
            instance Reader (EditReader edit) => Reader (ObjectReader edit) where
            {
                type ReaderSubject (ObjectReader edit) = EditSubject edit;
            };
            instance IOFullReader (EditReader edit) => IOFullReader (ObjectReader edit);
        |]);
    };

    type ObjectEdit edit = NoEdit (ObjectReader edit);

    objectEditLens :: forall f f' edita editb. (MonadOne f,Edit edita) => EditLens' f edita editb -> EditLens' f' (ObjectEdit edita) (ObjectEdit editb);
    objectEditLens lens = let
    {
        editGet :: ReadFunction (ObjectReader edita) (ObjectReader editb);
        editGet ReadObject = do
        {
            obja <- readable ReadObject;
            return $ fixedMapObject lens obja;
        };
        editUpdate edita = never edita;
        editLensFunction = MkEditFunction{..};
        editLensPutEdit editb = never editb;
    } in MkEditLens{..};
}
