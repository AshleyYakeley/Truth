module Data.Bijection where
{
    import Data.Chain;
    import Control.Category;
    import Prelude hiding (id,(.));

    data Bijection a b = MkBijection
    {
        biForwards :: a -> b,
        biBackwards :: b -> a
    };
    -- biForwards . biBackwards = id
    -- biBackwards . biForwards = id

    instance Category Bijection where
    -- "You people and your quaint little categories" -- Jack Harkness, Torchwood ep. 2
    {
        id = MkBijection id id;
        (MkBijection p1 q1) . (MkBijection p2 q2) = MkBijection (p1 . p2) (q2 . q1);
    };

    instance (Functor f) => CatFunctor Bijection f where
    {
        cfmap bi = MkBijection
        {
            biForwards = fmap (biForwards bi),
            biBackwards = fmap (biBackwards bi)
        };
    };

    biSwap :: Bijection (a,b) (b,a);
    biSwap = MkBijection swap swap where
    {
        swap (a,b) = (b,a);
    };
}
