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

    instance (SubjectReader ra,SubjectReader rb) => SubjectReader (EitherReader ra rb) where
    {
        type ReaderSubject (EitherReader ra rb) = Either (ReaderSubject ra) (ReaderSubject rb);

        -- readFromSubject :: ReaderSubject (OneReader f reader) -> (forall t. OneReader f reader t -> t);
        readFromSubject (Left _) EitherReadIsRight = False;
        readFromSubject (Right _) EitherReadIsRight = True;
        readFromSubject (Left a) (EitherReadLeft reader) = Just $ readFromSubject a reader;
        readFromSubject (Right _) (EitherReadLeft _) = Nothing;
        readFromSubject (Left _) (EitherReadRight _) = Nothing;
        readFromSubject (Right a) (EitherReadRight reader) = Just $ readFromSubject a reader;
    };

    instance (FullSubjectReader ra,FullSubjectReader rb) => FullSubjectReader (EitherReader ra rb) where
    {
        subjectFromReader = do
        {
            mleft <- mapEitherReadLeft subjectFromReader;
            mright <- mapEitherReadRight subjectFromReader;
            case (mleft,mright) of
            {
                (Just a,Nothing) -> return $ Left a;
                (Nothing,Just a) -> return $ Right a;
                _ -> error $ "pureFromReader: inconsistent EitherReader";
            };
        };
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
    };

    instance (InvertableEdit ea,InvertableEdit eb) => InvertableEdit (EitherEdit ea eb) where
    {
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
}
