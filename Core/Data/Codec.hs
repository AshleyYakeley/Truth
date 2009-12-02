module Data.Codec where
{
    import Data.Chain;
    import Data.Bijection;
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

    bijectionCodec :: Bijection a b -> Codec a b;
    bijectionCodec (MkBijection p q) = MkCodec (Just . p) q;

    instance (Traversable f) => CatFunctor Codec f where
    {
        cfmap codec = MkCodec
        {
            decode = traverse (decode codec),
            encode = fmap (encode codec)
        };
    };
}
