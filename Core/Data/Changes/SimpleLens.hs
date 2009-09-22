module Data.Changes.SimpleLens where
{
    import Data.Changes.WholeLens;
    import Data.Result;
    import Data.ConstFunction;
    import Data.Traversable;
    import Data.FunctorOne;
    import Data.Chain;
    import Control.Applicative;
    import Control.Category;
    import Prelude hiding (id,(.),sequence);

    -- | A SimpleLens is a FixedLens that doesn't bother with Edits.
    ;
    data SimpleLens' m a b = MkSimpleLens
    {
        simpleLensGet :: a -> b,
        simpleLensPutback :: b -> ConstFunction a (m a)
    };

    simpleLensModify :: SimpleLens' m a b -> (b -> b) -> a -> m a;
    simpleLensModify lens bb a = applyConstFunction (simpleLensPutback lens (bb (simpleLensGet lens a))) a;

    simpleLensMap :: (FunctorOne m) => SimpleLens' m a b -> (b -> b) -> (a -> a);
    simpleLensMap lens bb a = case getMaybeOne (simpleLensModify lens bb a) of
    {
        Just a' -> a';
        _ -> a;
    };

    type SimpleLens = SimpleLens' Maybe;

    toSimpleLens :: (FunctorOne m) => SimpleLens' m edita editb -> SimpleLens edita editb;
    toSimpleLens lens = MkSimpleLens
    {
        simpleLensGet = simpleLensGet lens,
        simpleLensPutback = \b -> do
        {
            ma <- simpleLensPutback lens b;
            return (getMaybeOne ma);
        }
    };

    instance (Applicative m,FunctorOne m) => Category (SimpleLens' m) where
    {
        id = MkSimpleLens
        {
            simpleLensGet = id,
            simpleLensPutback = \b -> pure (pure b)
        };
        bc . ab = MkSimpleLens
        {
            simpleLensGet = (simpleLensGet bc) . (simpleLensGet ab),
            simpleLensPutback = \c -> do
            {
                mb <- cofmap1CF (simpleLensGet ab) (simpleLensPutback bc c);
                case retrieveOne mb of
                {
                    SuccessResult b -> simpleLensPutback ab b;
                    FailureResult ff -> return ff;
                }
            }
        };
    };

    instance (FunctorOne f,Applicative m) => CatFunctor (SimpleLens' m) f where
    {
        cfmap lens = MkSimpleLens
        {
            simpleLensGet = fmap (simpleLensGet lens),
            simpleLensPutback = \fb -> do
            {
                ffa <- traverse (\b -> cfmap (simpleLensPutback lens b)) fb;
                return (sequenceA (joinOne ffa));
            }
        };
    };

    wholeSimpleLens :: WholeLens' m a b -> SimpleLens' m a b;
    wholeSimpleLens lens = MkSimpleLens
    {
        simpleLensGet = wholeLensGet lens,
        simpleLensPutback = \b -> pure (wholeLensPutback lens b)
    };
}
