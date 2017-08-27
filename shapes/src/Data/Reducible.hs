module Data.Reducible where
{
    import Prelude;
    import qualified Data.List as List;
    import qualified Data.Maybe as List;


    class Functor f => Reducible f where
    {
        mapMaybe :: (a -> Maybe b) -> f a -> f b;

        catMaybes :: f (Maybe a) -> f a;
        catMaybes = mapMaybe id;

        filter :: (a -> Bool) -> f a -> f a;
        filter test = mapMaybe $ \a -> if test a then Just a else Nothing;
    };

    instance Reducible [] where
    {
        mapMaybe = List.mapMaybe;
        catMaybes = List.catMaybes;
        filter = List.filter;
    };

    instance Reducible Maybe where
    {
        mapMaybe amb ma = ma >>= amb;
    };
}
