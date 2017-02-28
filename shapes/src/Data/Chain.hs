module Data.Chain where
{
--    import Data.Witness;
    import Control.Category;
    import Prelude hiding (id,(.));
    import Data.Kind;

    data Chain link (a :: k) (b :: k) where
    {
        EmptyChain :: Chain link t t;
        LinkChain :: link b c -> Chain link a b -> Chain link a c;
    };

    singleChain :: link a b -> Chain link a b;
    singleChain link = LinkChain link EmptyChain;

    instance Category (Chain (link :: k -> k -> Type)) where
    {
        id = EmptyChain;
        EmptyChain . chain = chain;
        (LinkChain link ab) . chain = LinkChain link (ab . chain);
    };
{-
    instance (SimpleWitness1 link) => SimpleWitness1 (Chain link) where
    {
        matchWitness1 EmptyChain EmptyChain = Just Refl;
        matchWitness1 (LinkChain link1 chain1) (LinkChain link2 chain2) = do
        {
            Refl <- testEquality chain1 chain2;
            matchWitness1 link1 link2;
        };
        matchWitness1 _ _ = Nothing;
    };
-}
    class CatFunctor t f where
    {
        cfmap :: t a b -> t (f a) (f b);
    };

    instance (Functor f) => CatFunctor (->) f where
    {
        cfmap = fmap;
    };

    instance CatFunctor link f => CatFunctor (Chain link) f where
    {
        cfmap EmptyChain = EmptyChain;
        cfmap (LinkChain link chain) = LinkChain (cfmap link) (cfmap chain);
    };

    class (Category k) => CategoryOr k where
    {
        codiag :: k (Either a a) a;
        (|||) :: k a c -> k b c -> k (Either a b) c;

        codiag = id ||| id;
    };

    instance CategoryOr (->) where
    {
        (|||) ac _ (Left a) = ac a;
        (|||) _ bc (Right b) = bc b;

        codiag (Left a) = a;
        codiag (Right a) = a;
    };
}
