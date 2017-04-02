module Data.Codec where
{
    import Prelude hiding (id,(.));
    import Control.Category;
    import Data.Bijection;
    import Data.Chain;
    import Data.Functor.Identity;
    import Data.Result;
    import Data.MonadOne;


    class IsBiMap bm where
    {
        mapBiMapM :: (forall x. m1 x -> m2 x) -> bm m1 a b -> bm m2 a b;
    };

    toBiMapMaybe :: (IsBiMap bm,MonadOne m) => bm m edita editb -> bm Maybe edita editb;
    toBiMapMaybe = mapBiMapM getMaybeOne;

    toBiMapResult :: forall e bm m edita editb. (IsBiMap bm,MonadOne m) => e -> bm m edita editb -> bm (Result e) edita editb;
    toBiMapResult e = mapBiMapM (mrf . retrieveOne) where
    {
        mrf :: Result (Limit m) a -> Result e a;
        mrf (SuccessResult a) = SuccessResult a;
        mrf (FailureResult _) = FailureResult e;
    };


    data Codec' m a b = MkCodec
    {
        decode :: a -> m b,
        encode :: b -> a
    };
    -- must have decode . encode = Just

    instance IsBiMap Codec' where
    {
        mapBiMapM ff codec = MkCodec
        {
            decode = ff . (decode codec),
            encode = encode codec
        };
    };

    type Codec = Codec' Maybe;

    instance (Monad m) => Category (Codec' m) where
    {
        id = MkCodec return id;
        (MkCodec bmc cb) . (MkCodec amb ba) = MkCodec (\a -> (amb a) >>= bmc) (ba . cb);
    };

    bijectionCodec :: Bijection a b -> Codec' Identity a b;
    bijectionCodec (MkBijection p q) = MkCodec (Identity . p) q;

    instance (Traversable f,Applicative m) => CatFunctor (Codec' m) f where
    {
        cfmap codec = MkCodec
        {
            decode = traverse (decode codec),
            encode = fmap (encode codec)
        };
    };
}
