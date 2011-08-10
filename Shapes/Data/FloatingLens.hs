module Data.FloatingLens where
{
    import Data.Lens;
    import Data.Codec;
    import Data.Traversable;
    import Data.FunctorOne;
    import Data.Chain;
    import Control.Applicative;

    data FloatingLens' state m a b = MkFloatingLens
    {
        floatingLensInitial :: state,
        floatingLensGet :: state -> a -> b,
        floatingLensPutback :: state -> b -> a -> m (state,a)
    };

    fixedFloatingLens :: (Functor m) => Lens' m a b -> FloatingLens' () m a b;
    fixedFloatingLens lens = MkFloatingLens
    {
        floatingLensInitial = (),
        floatingLensGet = \_ -> lensGet lens,
        floatingLensPutback = \_ b -> do
        {
            ma <- lensPutback lens b;
            return (fmap (\a -> ((),a)) ma);
        }
    };

    instance (FunctorOne f,Applicative m) => CatFunctor (FloatingLens' state m) f where
    {
        cfmap flens = MkFloatingLens
        {
            floatingLensInitial = floatingLensInitial flens,
            floatingLensGet = \state -> fmap (floatingLensGet flens state),
            floatingLensPutback = \state fb -> do
            {
                ffmsa <- traverse (\b -> cfmap (floatingLensPutback flens state b)) fb;
                return (fmap (\fsa -> (case getMaybeOne fsa of
                {
                    Just (newstate,_) -> newstate;
                    _ -> state;
                },fmap snd fsa)) (sequenceA (exec ffmsa)));
            }
        };
    };

    type FloatingLens state = FloatingLens' state Maybe;

    instance IsBiMap (FloatingLens' state) where
    {
        mapBiMapM ff flens = MkFloatingLens
        {
            floatingLensInitial = floatingLensInitial flens,
            floatingLensGet = floatingLensGet flens,
            floatingLensPutback = \state b -> do
            {
                msa <- floatingLensPutback flens state b;
                return (ff msa);
            }
        };
    };
}
