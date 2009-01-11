module Data.Codec where
{
	import Control.Category;
	import Data.Traversable;
	import Prelude hiding (id,(.));

	data Codec a b = MkCodec
	{
		decode :: a -> Maybe b,
		encode :: b -> a
	};
	-- must have decode . encode = Just
	
	instance Category Codec where
	{
		id = MkCodec Just id;
		(MkCodec bmc cb) . (MkCodec amb ba) = MkCodec (\a -> (amb a) >>= bmc) (ba . cb);
	};
	
	traversableCodec :: (Traversable f) => Codec a b -> Codec (f a) (f b);
	traversableCodec codec = MkCodec
	{
		decode = traverse (decode codec),
		encode = fmap (encode codec)
	};
}
