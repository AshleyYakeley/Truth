module Data.Chain where
{
    import Data.Witness;
    import Control.Category;
    import Prelude hiding (id,(.));

    data Chain link a b where
    {
        EmptyChain :: Chain link t t;
        LinkChain :: link b c -> Chain link a b -> Chain link a c;
    };
    
    singleChain :: link a b -> Chain link a b;
    singleChain link = LinkChain link EmptyChain;
    
    instance Category (Chain link) where
    {
        id = EmptyChain;
        EmptyChain . chain = chain;
        (LinkChain link ab) . chain = LinkChain link (ab . chain);
    };
    
    instance (SimpleWitness1 link) => SimpleWitness1 (Chain link) where
    {
        matchWitness1 EmptyChain EmptyChain = Just MkEqualType;
        matchWitness1 (LinkChain link1 chain1) (LinkChain link2 chain2) = do
        {
            MkEqualType <- matchWitness chain1 chain2;
            matchWitness1 link1 link2;
        };
        matchWitness1 _ _ = Nothing;
    };
    
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
}
