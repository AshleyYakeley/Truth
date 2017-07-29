module Truth.Core.Object.ObjectEdit where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.None;
    import Truth.Core.Object.MutableEdit;


    data ObjectReader edit t where
    {
        ReadObject :: ObjectReader edit (MutableEdit IO edit);
    };

    instance Reader (EditReader edit) => Reader (ObjectReader edit) where
    {
        type ReaderSubject (ObjectReader edit) = EditSubject edit;
        readFrom subj ReadObject = constantMutableEdit subj;
    };

    instance IOFullReader (EditReader edit) => GenFullReader MonadIO (ObjectReader edit) where
    {
        genFromReader = do
        {
            muted <- readable ReadObject;
            liftIO $ unReadable ioFromReader $ mutableRead muted;
        };
    };

    type ObjectEdit edit = NoEdit (ObjectReader edit);

    instance Reader (EditReader edit) => GenEdit MonadIO (ObjectEdit edit) where
    {
        applyEdit :: ObjectEdit edit -> EditReader (ObjectEdit edit) t -> GenReadable MonadIO (EditReader (ObjectEdit edit)) t;
        applyEdit = never;
    };

    -- instance GenFullEdit MonadIO (ObjectEdit edit);

{-
    $(return []);
    instance HasTypeInfo ObjectReader where
    {
        typeWitness = $(generateWitness [t|ObjectReader|]);
        typeName _ = "ObjectReader";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance Reader (EditReader edit) => Reader (ObjectReader edit) where
            {
                type ReaderSubject (ObjectReader edit) = EditSubject edit;
            };
            instance IOFullReader (EditReader edit) => GenFullReader MonadIO (ObjectReader edit);
        |]);
    };
-}
    objectEditLens :: forall f f' edita editb. (MonadOne f,Edit edita) => EditLens' f edita editb -> EditLens' f' (ObjectEdit edita) (ObjectEdit editb);
    objectEditLens lens = let
    {
        editGet :: ReadFunction (ObjectReader edita) (ObjectReader editb);
        editGet ReadObject = do
        {
            muted <- readable ReadObject;
            return $ fixedMapMutableEdit lens muted;
        };
        editUpdate edita = never edita;
        editLensFunction = MkEditFunction{..};
        editLensPutEdit editb = never editb;
    } in MkEditLens{..};
}
