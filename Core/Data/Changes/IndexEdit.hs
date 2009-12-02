module Data.Changes.IndexEdit where
{
    import Data.Changes.SimpleLens;
    import Data.Changes.Edit;
    import Data.Changes.HasTypeRep;
    import Data.Changes.EditRep;
    import Data.TypeKT.WitnessKT;
    import Data.TypeKT.IOWitnessKT;
    import Data.OpenWitness;
    import Data.Witness;
    import Data.ConstFunction;
    import Control.Arrow;
    import Prelude hiding (id,(.));

    data IndexEdit a i edit = MkIndexEdit i edit;

    class Container a where
    {
        type Index a;
        type Part a;

        sameIndex :: a -> Index a -> Index a -> Bool;
        indexLens :: Index a -> SimpleLens a (Part a);
    };

    data ContainerInst a where
    {
        MkContainerInst :: forall a. (Container a) => TypeT (Index a) -> TypeT (Part a) -> ContainerInst a;
    };

    instance TypeFactT ContainerInst where
    {
        witFactT = unsafeIOWitnessFromString "Data.Changes.IndexEdit.ContainerInst";
    };

    instance (Eq a) => Container (a -> b) where
    {
        type Index (a -> b) = a;
        type Part (a -> b) = b;

        sameIndex = \_ -> (==);

        indexLens a = MkSimpleLens
        {
            simpleLensGet = \ab -> ab a,
            simpleLensPutback = \b -> arr (\ab -> Just (\a' -> if a == a' then b else ab a'))
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

        indexLens i | i < 0 = MkSimpleLens
        {
            simpleLensGet = \_ -> Nothing,
            simpleLensPutback = \ma -> case ma of
            {
                Nothing -> arr Just;
                _ -> return Nothing;
            }
        };
        indexLens i = MkSimpleLens
        {
            simpleLensGet = elementGet i,
            simpleLensPutback = \ma -> case ma of
            {
                Just a -> arr (elementPutback i a);
                Nothing -> arr (\list -> if i < length list then Nothing else Just list);
            }
        };
    };

    instance (Edit edit,Container container,Part container ~ Maybe (Subject edit),index ~ Index container) =>
     Edit (IndexEdit container index edit) where
    {
        type Subject (IndexEdit container index edit) = container;

        applyEdit (MkIndexEdit i edita) = arr (simpleLensMap (indexLens i) (fmap (applyConstFunction (applyEdit edita))));

        invertEdit (MkIndexEdit i edita) oldcont = do
        {
            oldpart <- simpleLensGet (indexLens i) oldcont;
            invedita <- invertEdit edita oldpart;
            return (MkIndexEdit i invedita);
        };
    };

    instance HasTypeKTKTKTT IndexEdit where
    {
        typeKTKTKTT = MkTypeKTKTKTT
            (WitKTKTKTT (unsafeIOWitnessFromString "Data.Changes.IndexEdit.IndexEdit"))
            (mkTInfoKTKTKTT_ (witFactT :: IOWitness (SatKTT EditInst)) (\tcontainer tindex tedit -> do
                {
                    MkEditInst tsubj <- typeFactT tedit;
                    MkContainerInst tcindex tcpart <- typeFactT tcontainer;
                    MkEqualType <- matchWitnessT tindex tcindex;
                    MkEqualType <- matchWitnessT tcpart (applyTTypeT (typeKTT :: TypeKTT Maybe) tsubj);
                    return (MkEditInst tcontainer);
                })
            );
    };
}
