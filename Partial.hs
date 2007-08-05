module Partial where
{
	import Data.List;

	class Partial a p | p -> a where
	{
		toPartial  :: forall m. (Monad m) => a -> p m;
		getFromPartial :: forall m. (Monad m) => p m -> m a;
	};

	switchPartial :: (Partial a p1,Partial a p2,Monad m) => p1 m -> m (p2 m);
	switchPartial p = do
	{
		a <- getFromPartial p;
		return (toPartial a);
	};

	newtype Whole a (m :: * -> *) = MkWhole a;
	
	instance Partial a (Whole a) where
	{
		toPartial = MkWhole;
		getFromPartial (MkWhole a) = return a;
	};

	newtype Simple a (m :: * -> *) = MkSimple (m a);
	
	instance Partial a (Simple a) where
	{
		toPartial = MkSimple . return;
		getFromPartial (MkSimple ma) = ma;
	};

	data PartialList i m = MkPartialList
	{
		getPLLength :: m Int,
		getPLBlock :: Int -> Int -> m [i]
	};
	
	instance Partial [i] (PartialList i) where
	{
		toPartial list = MkPartialList (return (length list)) (\start count -> return (take count (drop start list)));
		getFromPartial pl = do
		{
			len <- getPLLength pl;
			getPLBlock pl 0 len;
		};
	};
}
