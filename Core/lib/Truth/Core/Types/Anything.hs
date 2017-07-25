module Truth.Core.Types.Anything where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.SumWhole;


    data Anything where
    {
        MkAnything :: forall (edit :: *). TypeInfo edit -> TypeInfo (EditReader edit) -> TypeInfo (EditSubject edit) -> EditSubject edit -> Anything;
    };

    instance HasTypeInfo Anything where
    {
        typeWitness = $(generateWitness [t|Anything|]);
        typeName _ = "Anything";
    };

    data AnyTypes where
    {
        MkAnyTypes :: forall (edit :: *). TypeInfo edit -> TypeInfo (EditReader edit) -> TypeInfo (EditSubject edit) -> AnyTypes;
    };

    data AnyReader t where
    {
        ReadAnyTypes :: AnyReader AnyTypes;
        ReadAnyReader :: forall reader t. Reader reader => TypeInfo reader -> reader t -> AnyReader (Maybe t);
    };

    instance Reader AnyReader where
    {
        type ReaderSubject AnyReader = Anything;

        readFrom (MkAnything ie ir ia _a) ReadAnyTypes = MkAnyTypes ie ir ia;
        readFrom (MkAnything _ie ir _ia a) (ReadAnyReader infor reader) = do
        {
            Refl <- testEquality ir infor;
            return (readFrom a reader);
        };
    };

    $(return []);
    instance HasTypeInfo AnyReader where
    {
        typeWitness = $(generateWitness [t|AnyReader|]);
        typeName _ = "AnyReader";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance Reader AnyReader where
            {
                type ReaderSubject AnyReader = Anything;
            };
        |]);
    };

    data AnyEdit where
    {
        MkAnyEdit :: forall edit. (Edit edit) => TypeInfo edit -> edit -> AnyEdit;
    };

    instance Floating AnyEdit AnyEdit where
    {
        floatingUpdate (MkAnyEdit info1 edit1) aedit2@(MkAnyEdit info2 edit2) = case testEquality info1 info2 of
        {
            Just Refl -> MkAnyEdit info2 $ floatingUpdate edit1 edit2;
            Nothing -> aedit2;
        };
    };

    instance Edit AnyEdit where
    {
        type EditReader AnyEdit = AnyReader;

        applyEdit (MkAnyEdit ie edit) areader@(ReadAnyReader ir reader) = do
        {
            MkAnyTypes oie oir _oia <- readable ReadAnyTypes;
            case (testEquality oie ie,testEquality oir ir) of
            {
                (Just Refl,Just Refl) -> mapReadableF (readable . ReadAnyReader ir) $ applyEdit edit reader;
                _ -> readable areader;
            }
        };
        applyEdit (MkAnyEdit _ _) ReadAnyTypes = readable ReadAnyTypes; -- edit cannot change types

        -- invertEdit :: AnyEdit -> Readable AnyReader [AnyEdit];
        invertEdit (MkAnyEdit ie edit) = do
        {
            MkAnyTypes oie oir _oia <- readable ReadAnyTypes;
            case testEquality oie ie of
            {
                Just Refl -> do
                {
                    minvedits <- mapReadableF (readable . ReadAnyReader oir) $ invertEdit edit;
                    case minvedits of
                    {
                        Nothing -> return [];
                        Just invedits -> return $ fmap (MkAnyEdit ie) invedits;
                    };
                };
                Nothing -> return [];
            };
        };
    };

    $(return []);
    instance HasTypeInfo AnyEdit where
    {
        typeWitness = $(generateWitness [t|AnyEdit|]);
        typeName _ = "AnyEdit";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance Edit AnyEdit where
            {
                type EditReader AnyEdit = AnyReader;
            };
        |]);
    };

    type AnyWholeEdit = SumWholeEdit AnyEdit;
}
