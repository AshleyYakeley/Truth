module Truth.Core.Types.Either where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;


    data EitherReader (ra :: * -> *) (rb :: * -> *) (t :: *) where
    {
        EitherReadIsRight :: EitherReader ra rb Bool;
        EitherReadLeft :: ra t -> EitherReader ra rb (Maybe t);
        EitherReadRight :: rb t -> EitherReader ra rb (Maybe t);
    };

    mapEitherReadLeft :: MapReadable readable => readable ra t -> readable (EitherReader ra rb) (Maybe t);
    mapEitherReadLeft = mapReadableF (readable . EitherReadLeft);

    mapEitherReadRight :: MapReadable readable => readable rb t -> readable (EitherReader ra rb) (Maybe t);
    mapEitherReadRight = mapReadableF (readable . EitherReadRight);

    instance (Reader ra,Reader rb) => Reader (EitherReader ra rb) where
    {
        type ReaderSubject (EitherReader ra rb) = Either (ReaderSubject ra) (ReaderSubject rb);

        -- readFrom :: ReaderSubject (OneReader f reader) -> (forall t. OneReader f reader t -> t);
        readFrom (Left _) EitherReadIsRight = False;
        readFrom (Right _) EitherReadIsRight = True;
        readFrom (Left a) (EitherReadLeft reader) = Just $ readFrom a reader;
        readFrom (Right _) (EitherReadLeft _) = Nothing;
        readFrom (Left _) (EitherReadRight _) = Nothing;
        readFrom (Right a) (EitherReadRight reader) = Just $ readFrom a reader;
    };

    instance (IOFullReader ra,IOFullReader rb) => IOFullReader (EitherReader ra rb) where
    {
        ioFromReader = do
        {
            mleft <- mapEitherReadLeft ioFromReader;
            mright <- mapEitherReadRight ioFromReader;
            case (mleft,mright) of
            {
                (Just a,Nothing) -> return $ Left a;
                (Nothing,Just a) -> return $ Right a;
                _ -> fail $ "fromReader: inconsistent EitherReader";
            };
        };
    };

    instance (FullReader ra,FullReader rb) => FullReader (EitherReader ra rb) where
    {
        fromReader = do
        {
            mleft <- mapEitherReadLeft fromReader;
            mright <- mapEitherReadRight fromReader;
            case (mleft,mright) of
            {
                (Just a,Nothing) -> return $ Left a;
                (Nothing,Just a) -> return $ Right a;
                _ -> fail $ "fromReader: inconsistent EitherReader";
            };
        };
    };

    $(return []);
    instance HasTypeInfo EitherReader where
    {
        typeWitness = $(generateWitness [t|EitherReader|]);
        typeName _ = "EitherReader";
        typeKnowledge _ = $(declInfo [d|
            instance (Reader ra,Reader rb) => Reader (EitherReader ra rb) where
            {
                type ReaderSubject (EitherReader ra rb) = Either (ReaderSubject ra) (ReaderSubject rb);
            };
            instance (IOFullReader ra,IOFullReader rb) => IOFullReader (EitherReader ra rb);
            instance (FullReader ra,FullReader rb) => FullReader (EitherReader ra rb);
        |]);
    };

    -- note this edit cannot switch the subject between the left and right branches
    data EitherEdit ea eb = EitherEditLeft ea | EitherEditRight eb;

    instance (Floating ea ea,Floating eb eb) => Floating (EitherEdit ea eb) (EitherEdit ea eb) where
    {
        floatingUpdate (EitherEditLeft e1) (EitherEditLeft e2) = EitherEditLeft $ floatingUpdate e1 e2;
        floatingUpdate (EitherEditRight e1) (EitherEditRight e2) = EitherEditRight $ floatingUpdate e1 e2;
        floatingUpdate _ t = t;
    };

    instance (Edit ea,Edit eb) => Edit (EitherEdit ea eb) where
    {
        type EditReader (EitherEdit ea eb) = EitherReader (EditReader ea) (EditReader eb);

        applyEdit (EitherEditLeft edit) (EitherReadLeft reader) = mapEitherReadLeft $ applyEdit edit reader;
        applyEdit (EitherEditRight edit) (EitherReadRight reader) = mapEitherReadRight $ applyEdit edit reader;
        applyEdit _ reader = readable reader;

        invertEdit (EitherEditLeft edit) = do
        {
            medits <- mapEitherReadLeft $ invertEdit edit;
            case medits of
            {
                Just edits -> return $ fmap EitherEditLeft edits;
                Nothing -> return [];
            };
        };
        invertEdit (EitherEditRight edit) = do
        {
            medits <- mapEitherReadRight $ invertEdit edit;
            case medits of
            {
                Just edits -> return $ fmap EitherEditRight edits;
                Nothing -> return [];
            };
        };
    };

    $(return []);
    instance HasTypeInfo EitherEdit where
    {
        typeWitness = $(generateWitness [t|EitherEdit|]);
        typeName _ = "EitherEdit";
        typeKnowledge _ = $(declInfo [d|
            instance (Edit ea,Edit eb) => Edit (EitherEdit ea eb) where
            {
                type EditReader (EitherEdit ea eb) = EitherReader (EditReader ea) (EditReader eb);
            };
        |]);
    };
}
