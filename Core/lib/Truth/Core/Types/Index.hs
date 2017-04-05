module Truth.Core.Types.Index where
{
{-
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.None;
    import Truth.Core.Types.Either;


    class Container container where
    {
        type Index container :: *;
        type Part container :: *;

        sameIndex :: forall reader. (FullReader reader,ReaderSubject reader ~ container) =>
            Index container -> Index container -> Readable reader Bool;
        indexLens :: forall partedit. (Edit partedit, EditSubject partedit ~ Part container) =>
            Index container -> FloatingEditLens (Index container) (IndexEdit container edit) (PartEdit edit);
    };

    instance (Eq a) => Container (a -> b) where
    {
        type Index (a -> b) = a;
        type Part (a -> b) = b;

        sameIndex a1 a2 = return (a1 == a2);

        indexLens a = MkLens
        {
            lensGet = \ab -> ab a,
            lensPutback = \b -> arr (\ab -> Just (\a' -> if a == a' then b else ab a'))
        };
    };

    elementModify :: Int -> (e -> e) -> [e] -> [e];
    elementModify 0 f (e:es) = (f e):es;
    elementModify _ _ [] = [];
    elementModify i f (e:es) = e:(elementModify (i - 1) f es);

    elementGet :: Int -> [e] -> Maybe e;
    elementGet 0 (e:_) = Just e;
    elementGet _ [] = Nothing;
    elementGet i (_:es) = elementGet (i - 1) es;

    elementPutback :: Int -> e -> [e] -> Maybe [e];
    elementPutback _ _ [] = Nothing;
    elementPutback 0 x (_:es) = Just (x:es);
    elementPutback i x (e:es) = do
    {
        xs <- elementPutback (i - 1) x es;
        return (e:xs);
    };

    instance Container [a] where
    {
        type Index [a] = Int;
        type Part [a] = Maybe a;

        sameIndex = \_ -> (==);

        indexLens i | i < 0 = MkLens
        {
            lensGet = \_ -> Nothing,
            lensPutback = \ma -> case ma of
            {
                Nothing -> arr Just;
                _ -> return Nothing;
            }
        };
        indexLens i = MkLens
        {
            lensGet = elementGet i,
            lensPutback = \ma -> case ma of
            {
                Just a -> arr (elementPutback i a);
                Nothing -> arr (\list -> if i < length list then Nothing else Just list);
            }
        };
    };

    data Container_Inst a where
    {
        Container_Inst :: forall a. (Container a) => Info (Type_T (Index a)) -> Info (Type_T (Part a)) -> Container_Inst (Type_T a);
    };
    $(factInstances [t|Container_Inst|]);


    data IndexReader container reader t where
    {
        ReadIndex :: reader t -> (Index container) -> IndexReader container reader t;
    };

        -- indexLens :: index -> Lens container part;

    instance (Reader reader,ReaderSubject reader ~ Part container) => Reader (IndexReader container reader) where
    {
        type ReaderSubject (IndexReader container reader) = container;

        -- readFrom :: container -> (forall t. IndexReader container reader t -> t);
        readFrom container (ReadIndex reader index) = readFrom (lensGet (indexLens index) container) reader;


        -- fromReader :: forall m. (Monad m) => (forall t. IndexReader container reader t -> m t) -> m container;
        -- fromReader read =
    };

    data IndexEdit container edit = MkIndexEdit (Index container) edit;

    instance (Eq (Index container),Edit edit,Container container,Part container ~ Maybe (EditSubject edit)) =>
     Edit (IndexEdit container edit) where
    {
        type EditReader (IndexEdit container edit) = IndexReader container (EditReader edit);

        -- applyEdit :: IndexEdit container edit ->
        --   IndexReader container (EditReader edit) t -> Readable (IndexReader container (EditReader edit)) t;
        applyEdit (MkIndexEdit i edita) (ReadIndex reader i') =





        arr (lensMap (indexLens i) (fmap (applyConstFunction (applyEdit edita))));

        invertEdit (MkIndexEdit i edita) oldcont = do
        {
            oldpart <- lensGet (indexLens i) oldcont;
            invedita <- invertEdit edita oldpart;
            return (MkIndexEdit i invedita);
        };

        updateEdit (MkIndexEdit i' edit') (MkIndexEdit i edit) | i' == i = MkIndexEdit i (updateEdit edit' edit);
        updateEdit _ edit = edit;
    };

    instance HasInfo (Type_KTKTT IndexEdit) where
    {
        info = mkSimpleInfo $(iowitness[t| Type_KTKTT IndexEdit |])
        [
            -- instance (Eq (Index container),Edit edit,Container container,Part container ~ Maybe (ReaderSubject edit)) =>
            --  Edit (IndexEdit container edit)
            mkFacts (MkFactS (\tcontainer -> MkFactS (\tedit -> MkFactZ (do
            {
                Edit_Inst tsubj <- matchProp $(type1[t|Edit_Inst|]) tedit;
                Container_Inst tindex tpart <- matchProp $(type1[t|Container_Inst|]) tcontainer;
                Eq_Inst <- matchProp $(type1[t|Eq_Inst|]) tindex;
                Refl <- testEquality tpart (applyInfo (info :: Info (Type_KTT Maybe)) tsubj);
                return (Edit_Inst tcontainer);
            })))
            :: FactS (FactS FactZ) Edit_Inst (Type_KTKTT IndexEdit)
            )
        ];
    };

    class (Edit edit) => FloatingPointer ptr edit where
    {
        updatePointer :: edit -> ptr -> Readable (EditReader edit) ptr;
    };

    instance FloatingPointer ptr (NoEdit subj) where
    {
        updatePointer (MkNoEdit edit) = never edit;
    };

    instance (FloatingPointer ptr edita,FloatingPointer ptr editb,ReaderSubject edita ~ ReaderSubject editb) =>
     FloatingPointer ptr (EitherEdit edita editb) where
    {
        updatePointer (LeftEdit edit) = updatePointer edit;
        updatePointer (RightEdit edit) = updatePointer edit;
    };

    instance (Eq index,Edit edit,Container container,Part container ~ Maybe (ReaderSubject edit),index ~ Index container)
     => FloatingPointer index (IndexEdit container edit) where
    {
        updatePointer _ ptr = return ptr;
    }
-}
}
