module Data.ConstFunction where
{
    import Prelude hiding (id,(.));
    import Control.Category;
    import Control.Arrow;
    import Data.Chain;


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

    class (Functor f) => FunctorGetPure f where
    {
        getPure :: forall a b. ConstFunction (f a) (b -> f b);
        getPure = FunctionConstFunction (\fa b -> fmap (const b) fa);
    };

    applicativeGetPure :: (Applicative f) => ConstFunction (f a) (b -> f b);
    applicativeGetPure = ConstConstFunction pure;

    instance FunctorGetPure ((->) p) where
    {
        getPure = applicativeGetPure;
    };

    instance (FunctorGetPure f) => CatFunctor ConstFunction f where
    {
        cfmap (ConstConstFunction b) = fmap (\bfb -> bfb b) getPure;
        cfmap (FunctionConstFunction ab) = FunctionConstFunction (fmap ab);
    };
}
