{-# OPTIONS -fno-warn-orphans #-}
module Data.FunctorOne where
{
    import Data.Chain;
    import Data.ConstFunction;
    import Data.Result;
    import Control.Applicative;
    import Control.Monad.Identity;
    import Control.Monad.Instances();

    class (Functor f) => FunctorOne f where
    {
        retrieveOne :: f a -> Result (forall b. f b) a;
        getPureOne :: ConstFunction (f a) (b -> f b);
    
        getMaybeOne :: f a -> Maybe a;
        getMaybeOne fa = resultToMaybe (retrieveOne fa);
    };
    -- retrieveOne (fmap f w) = fmap f (retrieveOne w)
    -- case (retrieveOne w) of {Left w' -> w';Right a -> fmap (\_ -> a) w;} = w

    instance Applicative Identity where
    {
        pure = Identity;
        (Identity f) <*> (Identity a) = Identity (f a);
    };

    instance FunctorOne Identity where
    {
        retrieveOne (Identity a) = SuccessResult a;
        getPureOne = return Identity;
        getMaybeOne (Identity a) = Just a;
    };

    instance FunctorOne Maybe where
    {
        retrieveOne (Just a) = SuccessResult a;
        retrieveOne Nothing = FailureResult Nothing;
        getPureOne = return pure;
        getMaybeOne = id;
    };
    
    instance FunctorOne (Either a) where
    {
        retrieveOne (Right b) = SuccessResult b;
        retrieveOne (Left a) = FailureResult (Left a);
        getPureOne = return Right;
    };

    instance FunctorOne ((,) p) where
    {
        retrieveOne (_,a) = SuccessResult a;
        getPureOne = FunctionConstFunction (\fa b -> fmap (const b) fa);
    };

    instance FunctorOne (Result e) where
    {
        retrieveOne (SuccessResult a) = SuccessResult a;
        retrieveOne (FailureResult e) = FailureResult (FailureResult e);
        getPureOne = return pure;
        getMaybeOne = resultToMaybe;
    };
    
    -- not quite as general as (->) which has (Functor f)
    instance (FunctorOne f) => CatFunctor ConstFunction f where
    {
        cfmap (FunctionConstFunction ab) = FunctionConstFunction (fmap ab);
        cfmap (ConstConstFunction b) = do
        {
            bfb <- getPureOne;
            return (bfb b);
        };
    };
}
