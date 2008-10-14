module Codec where
{
	import Control.Category;
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
}
