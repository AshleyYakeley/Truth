module Data.FunctorOne where
{
	import Data.Result;
	import Control.Monad.Instances();

	class (Functor f) => FunctorOne f where
	{
		retrieveOne :: f a -> Either (forall b. f b) a;
	};
	-- retrieveOne (fmap f w) = fmap f (retrieveOne w)
	-- case (retrieveOne w) of {Left w' -> w';Right a -> fmap (\_ -> a) w;} = w

	instance FunctorOne Maybe where
	{
		retrieveOne (Just a) = Right a;
		retrieveOne Nothing = Left Nothing;
	};
	
	instance FunctorOne (Either a) where
	{
		retrieveOne (Right b) = Right b;
		retrieveOne (Left a) = Left (Left a);
	};

	instance FunctorOne ((,) p) where
	{
		retrieveOne (_,a) = Right a;
	};

	instance FunctorOne (Result e) where
	{
		retrieveOne (SuccessResult a) = Right a;
		retrieveOne (FailureResult e) = Left (FailureResult e);
	};
}
