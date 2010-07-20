module Truth.Edit.IndexEdit where
{
    import Truth.Edit.NoEdit;
    import Truth.Edit.Either();
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    class Container a where
    {
        type Index a;
        type Part a;

        sameIndex :: a -> Index a -> Index a -> Bool;
        indexLens :: Index a -> Lens a (Part a);
    };

    instance (Eq a) => Container (a -> b) where
    {
        type Index (a -> b) = a;
        type Part (a -> b) = b;

        sameIndex = \_ -> (==);

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

    data IndexEdit container edit = MkIndexEdit (Index container) edit;

    instance (Eq (Index container),Edit edit,Container container,Part container ~ Maybe (Subject edit)) =>
     Edit (IndexEdit container edit) where
    {
        type Subject (IndexEdit container edit) = container;

        applyEdit (MkIndexEdit i edita) = arr (lensMap (indexLens i) (fmap (applyConstFunction (applyEdit edita))));

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
            -- instance (Eq (Index container),Edit edit,Container container,Part container ~ Maybe (Subject edit)) =>
            --  Edit (IndexEdit container edit)
            mkFacts (MkFactS (\tcontainer -> MkFactS (\tedit -> MkFactZ (do
            {
                Edit_Inst tsubj <- matchProp $(type1[t|Edit_Inst|]) tedit;
                Container_Inst tindex tpart <- matchProp $(type1[t|Container_Inst|]) tcontainer;
                Eq_Inst <- matchProp $(type1[t|Eq_Inst|]) tindex;
                MkEqualType <- matchWitness tpart (applyInfo (info :: Info (Type_KTT Maybe)) tsubj);
                return (Edit_Inst tcontainer);
            })))
            :: FactS (FactS FactZ) Edit_Inst (Type_KTKTT IndexEdit)
            )
        ];
    };

    class (Edit edit) => FloatingPointer ptr edit where
    {
        updatePointer :: Subject edit -> edit -> ptr -> ptr;
    };

    instance FloatingPointer ptr (NoEdit subj) where
    {
        updatePointer _subj (MkNoEdit edit) _ptr = never edit;
    };

    instance (FloatingPointer ptr edita,FloatingPointer ptr editb,Subject edita ~ Subject editb) =>
     FloatingPointer ptr (Either edita editb) where
    {
        updatePointer subj (Left edit) ptr = updatePointer subj edit ptr;
        updatePointer subj (Right edit) ptr = updatePointer subj edit ptr;
    };

    instance (Eq index,Edit edit,Container container,Part container ~ Maybe (Subject edit),index ~ Index container)
     => FloatingPointer index (IndexEdit container edit) where
    {
        updatePointer _ _ ptr = ptr;
    }
}
