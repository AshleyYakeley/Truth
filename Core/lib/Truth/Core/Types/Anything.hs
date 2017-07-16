module Truth.Core.Types.Anything where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.SumWhole;


    data Anything where
    {
        MkAnything :: forall (edit :: *). Info edit -> Info (EditReader edit) -> Info (EditSubject edit) -> EditSubject edit -> Anything;
    };

    instance HasInfo Anything where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Anything|]) [$(declInfo [d|
        |])];
    };

    data AnyTypes where
    {
        MkAnyTypes :: forall (edit :: *). Info edit -> Info (EditReader edit) -> Info (EditSubject edit) -> AnyTypes;
    };

    data AnyReader t where
    {
        ReadAnyTypes :: AnyReader AnyTypes;
        ReadAnyReader :: forall reader t. Reader reader => Info reader -> reader t -> AnyReader (Maybe t);
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
    instance HasInfo AnyReader where
    {
        info = mkSimpleInfo $(ionamedwitness[t|AnyReader|]) [$(declInfo [d|
            instance Reader AnyReader where
            {
                type ReaderSubject AnyReader = Anything;
            };
        |])];
    };

    data AnyEdit where
    {
        MkAnyEdit :: forall edit. (Edit edit) => Info edit -> edit -> AnyEdit;
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
    instance HasInfo AnyEdit where
    {
        info = mkSimpleInfo $(ionamedwitness[t|AnyEdit|]) [$(declInfo [d|
            instance Edit AnyEdit where
            {
                type EditReader AnyEdit = AnyReader;
            };
        |])];
    };

    type AnyWholeEdit = SumWholeEdit AnyEdit;
}
