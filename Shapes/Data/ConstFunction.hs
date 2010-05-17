module Data.ConstFunction where
{
    import Data.Chain;
    import Control.Instances();
    import Control.Arrow;
    import Control.Category;
    import Control.Applicative;
    import Prelude hiding (id,(.));

    data ConstFunction a b =
        ConstConstFunction b
        | FunctionConstFunction (a -> b);

    applyConstFunction :: ConstFunction a b -> a -> b;
    applyConstFunction (ConstConstFunction b) _ = b;
    applyConstFunction (FunctionConstFunction ab) a = ab a;

    applyConstFunctionA :: (Applicative m) => ConstFunction a b -> m a -> m b;
    applyConstFunctionA (ConstConstFunction b) _ = pure b;
    applyConstFunctionA (FunctionConstFunction ab) ma = fmap ab ma;

    instance Functor (ConstFunction t) where
    {
        fmap ab (ConstConstFunction a) = ConstConstFunction (ab a);
        fmap ab (FunctionConstFunction ta) = FunctionConstFunction (ab . ta);
    };

    cofmap1CF :: (p -> q) -> ConstFunction q a -> ConstFunction p a;
    cofmap1CF _ (ConstConstFunction a) = ConstConstFunction a;
    cofmap1CF pq (FunctionConstFunction qa) = FunctionConstFunction (qa . pq);

    instance Applicative (ConstFunction t) where
    {
        pure = ConstConstFunction;

        (ConstConstFunction ab) <*> fta = fmap ab fta;
        (FunctionConstFunction tab) <*> fta = FunctionConstFunction (\t -> tab t (applyConstFunction fta t));
    };

    instance Monad (ConstFunction t) where
    {
        return = ConstConstFunction;

        (ConstConstFunction a) >>= amb = amb a;
        (FunctionConstFunction ta) >>= amb = FunctionConstFunction (\t -> applyConstFunction (amb (ta t)) t);
    };

    instance Category ConstFunction where
    {
        id = FunctionConstFunction id;

        (ConstConstFunction c) . _ = ConstConstFunction c;
        (FunctionConstFunction bc) . fab = fmap bc fab;
    };

    instance Arrow ConstFunction where
    {
        arr = FunctionConstFunction;

        first abc = FunctionConstFunction (\(b,d) -> (applyConstFunction abc b,d));
    };

    instance (Applicative f) => CatFunctor ConstFunction f where
    {
        cfmap (ConstConstFunction b) = ConstConstFunction (pure b);
        cfmap (FunctionConstFunction ab) = FunctionConstFunction (fmap ab);
    };
}
