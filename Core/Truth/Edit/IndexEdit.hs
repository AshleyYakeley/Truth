module Truth.Edit.IndexEdit where
{
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

    data ContainerInst a where
    {
        MkContainerInst :: forall a. (Container a) => InfoT (Index a) -> InfoT (Part a) -> ContainerInst a;
    };

    instance PropertyT ContainerInst where
    {
        matchPropertyT = matchPropertyT_Fact;
    };

    instance FactT ContainerInst where
    {
        witFactT = unsafeIOWitnessFromString "Truth.Edit.IndexEdit.ContainerInst";
    };

    data IndexEdit a i edit = MkIndexEdit i edit;

    instance (Edit edit,Container container,Part container ~ Maybe (Subject edit),index ~ Index container) =>
     Edit (IndexEdit container index edit) where
    {
        type Subject (IndexEdit container index edit) = container;

        applyEdit (MkIndexEdit i edita) = arr (lensMap (indexLens i) (fmap (applyConstFunction (applyEdit edita))));

        invertEdit (MkIndexEdit i edita) oldcont = do
        {
            oldpart <- lensGet (indexLens i) oldcont;
            invedita <- invertEdit edita oldpart;
            return (MkIndexEdit i invedita);
        };
    };

    instance HasInfoKTKTKTT IndexEdit where
    {
        infoKTKTKTT = MkInfoKTKTKTT
            (WitKTKTKTT (unsafeIOWitnessFromString "Truth.Edit.IndexEdit.IndexEdit"))
            (mkTFactsKTKTKTT_ (witFactT :: IOWitness (SatKTT EditInst)) (\tcontainer tindex tedit -> do
                {
                    MkEditInst tsubj <- matchPropertyT tedit;
                    MkContainerInst tcindex tcpart <- matchPropertyT tcontainer;
                    MkEqualType <- matchWitnessT tindex tcindex;
                    MkEqualType <- matchWitnessT tcpart (applyTInfoT (infoKTT :: InfoKTT Maybe) tsubj);
                    return (MkEditInst tcontainer);
                })
            );
    };
}
