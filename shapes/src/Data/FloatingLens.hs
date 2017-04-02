module Data.FloatingLens where
{
    import Prelude hiding (id,(.));
    import Control.Category;
    import Data.Lens;
    import Data.Codec;
    import Data.MonadOne;
    import Data.Chain;


    data FloatingLens' initial m a b = forall state. MkFloatingLens
    {
        floatingLensInitial :: initial -> state,
        floatingLensGet :: state -> a -> b,
        floatingLensPutback :: state -> b -> a -> m (state,a)
    };

    fixedFloatingLens :: (Functor m) => Lens' m a b -> FloatingLens' initial m a b;
    fixedFloatingLens lens = MkFloatingLens
    {
        floatingLensInitial = \_ -> (),
        floatingLensGet = \_ -> lensGet lens,
        floatingLensPutback = \_ b -> do
        {
            ma <- lensPutback lens b;
            return (fmap (\a -> ((),a)) ma);
        }
    };

    instance Monad m => Category (FloatingLens' initial m) where
    {
        id = let
        {
            floatingLensInitial _ = ();
            floatingLensGet _ = id;
            floatingLensPutback _ b _ = return ((),b);
        } in MkFloatingLens{..};
        (MkFloatingLens iBC gBC pBC) . (MkFloatingLens iAB gAB pAB) = MkFloatingLens
        {
            floatingLensInitial = \initial -> (iAB initial,iBC initial),
            floatingLensGet = \(stateAB,stateBC) -> (gBC stateBC) . (gAB stateAB),
            floatingLensPutback = \(oldStateAB,oldStateBC) c oldA -> do
            {
                let
                {
                    oldB = gAB oldStateAB oldA;
                };
                (newStateBC,newB) <- pBC oldStateBC c oldB;
                (newStateAB,newA) <- pAB oldStateAB newB oldA;
                return ((newStateAB,newStateBC),newA);
            }
        };
    };

    instance (MonadOne f,Applicative m) => CatFunctor (FloatingLens' initial m) f where
    {
        cfmap (MkFloatingLens i g p) = MkFloatingLens
        {
            floatingLensInitial = i,
            floatingLensGet = \state -> fmap (g state),
            floatingLensPutback = \state fb -> do
            {
                ffmsa <- traverse (\b -> cfmap (p state b)) fb;
                return $ fmap (\fsa -> (case getMaybeOne fsa of
                {
                    Just (newstate,_) -> newstate;
                    _ -> state;
                },fmap snd fsa)) (sequenceA (ffmsa >>= id));
            }
        };
    };

    type FloatingLens initial = FloatingLens' initial Maybe;

    instance IsBiMap (FloatingLens' initial) where
    {
        mapBiMapM ff (MkFloatingLens i g p) = MkFloatingLens
        {
            floatingLensInitial = i,
            floatingLensGet = g,
            floatingLensPutback = \state b -> do
            {
                msa <- p state b;
                return (ff msa);
            }
        };
    };
}
