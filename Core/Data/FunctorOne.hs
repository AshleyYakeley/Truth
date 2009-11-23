{-# OPTIONS -fno-warn-orphans #-}
module Data.FunctorOne where
{
    import Data.Changes.EditRep;
    import Data.TypeKT.IOWitnessKT;
    import Data.OpenWitness;
    import Data.Chain;
    import Data.ConstFunction;
    import Data.Traversable;
    import Data.Foldable;
    import Data.Monoid;
    import Data.Result;
    import Control.Applicative;
    import Control.Monad.Identity;
    import Control.Monad.Instances();

    instance Foldable Identity where
    {
        foldMap am (Identity a) = am a;
    };

    instance Traversable Identity where
    {
        traverse amb (Identity a) = fmap Identity (amb a);
        sequenceA (Identity ma) = fmap Identity ma;
        mapM amb (Identity a) = liftM Identity (amb a);
        sequence (Identity ma) = liftM Identity ma;
    };

    instance Foldable (Either p) where
    {
        foldMap _ (Left _) = mempty;
        foldMap am (Right a) = am a;
    };

    instance Traversable (Either p) where
    {
        traverse _ (Left p) = pure (Left p);
        traverse amb (Right a) = fmap Right (amb a);

        sequenceA (Left p) = pure (Left p);
        sequenceA (Right ma) = fmap Right ma;

        mapM _ (Left p) = return (Left p);
        mapM amb (Right a) = liftM Right (amb a);

        sequence (Left p) = return (Left p);
        sequence (Right ma) = liftM Right ma;
    };

    instance Foldable ((,) p) where
    {
        foldMap am (_,a) = am a;
    };

    instance Traversable ((,) p) where
    {
        traverse amb (p,a) = fmap ((,) p) (amb a);
        sequenceA (p,ma) = fmap ((,) p) ma;
        mapM amb (p,a) = liftM ((,) p) (amb a);
        sequence (p,ma) = liftM ((,) p) ma;
    };

    class (Traversable f) => FunctorOne f where
    {
        retrieveOne :: f a -> Result (forall b. f b) a;
        getPureOne :: ConstFunction (f a) (b -> f b);

        getMaybeOne :: f a -> Maybe a;
        getMaybeOne fa = resultToMaybe (retrieveOne fa);
    };
    -- retrieveOne (fmap f w) = fmap f (retrieveOne w)
    -- case (retrieveOne w) of {Left w' -> w';Right a -> fmap (\_ -> a) w;} = w

    traverseOne :: (FunctorOne f,Applicative m) => (a -> m b) -> f a -> m (f b);
    traverseOne amb fa = case retrieveOne fa of
    {
        SuccessResult a -> fmap (\b -> fmap (\_ -> b) fa) (amb a);
        FailureResult fx -> pure fx;
    };

    sequenceAOne :: (FunctorOne f,Applicative m) => f (m a) -> m (f a);
    sequenceAOne fma = case retrieveOne fma of
    {
        SuccessResult ma -> fmap (\b -> fmap (\_ -> b) fma) ma;
        FailureResult fx -> pure fx;
    };

    bindOne :: (FunctorOne f) => f a -> (a -> f b) -> f b;
    bindOne fa afb = case retrieveOne fa of
    {
        SuccessResult a -> afb a;
        FailureResult fx -> fx;
    };

    joinOne :: (FunctorOne f) => f (f a) -> f a;
    joinOne ffa = case retrieveOne ffa of
    {
        SuccessResult fa -> fa;
        FailureResult fx -> fx;
    };

    data FunctorOneInst f where
    {
        MkFunctorOneInst :: forall f. (FunctorOne f) => FunctorOneInst f;
    };

    instance TypeFactKTT FunctorOneInst where
    {
        witFactKTT = unsafeIOWitnessFromString "Data.FunctorOne.FunctorOneInst";
    };

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
