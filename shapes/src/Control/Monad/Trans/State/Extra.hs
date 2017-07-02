module Control.Monad.Trans.State.Extra
(
    module Control.Monad.Trans.State,
    module Control.Monad.Trans.State.Extra
) where
{
    import Data.Functor.Identity;
    import Control.Monad.Trans.Class;
    import Control.Monad.Trans.State;
    import Control.Monad.Tunnel;
    import Data.Lens;


    unitStateT :: Functor m => m a -> StateT () m a;
    unitStateT ma = StateT $ \() -> fmap (\a -> (a,())) ma;

    runUnitStateT :: Functor m => StateT () m a -> m a;
    runUnitStateT sma = fmap fst $ runStateT sma ();

    swap3 :: (a,(b,c)) -> ((a,b),c);
    swap3 (a,(b,c)) = ((a,b),c);

    swap3' :: ((a,b),c) -> (a,(b,c));
    swap3' ((a,b),c) = (a,(b,c));

    revertStateT :: Monad m => StateT s m a -> StateT s m a;
    revertStateT sma = do
    {
        s <- get;
        a <- sma;
        put s;
        return a;
    };

    swapStateT :: Functor m => StateT s1 (StateT s2 m) a -> StateT s2 (StateT s1 m) a;
    swapStateT (StateT s1s2a) = StateT $ \olds2 -> StateT $ \olds1 -> fmap (\((a,news1),news2) -> ((a,news2),news1)) $ runStateT (s1s2a olds1) olds2;

    joinStateT :: Functor m => StateT s1 (StateT s2 m) a -> StateT (s1,s2) m a;
    joinStateT (StateT s1s2a) = StateT $ \(olds1,olds2) -> fmap swap3' $ runStateT (s1s2a olds1) olds2;

    splitStateT :: Functor m => StateT (s1,s2) m a -> StateT s1 (StateT s2 m) a;
    splitStateT (StateT s1s2a) = StateT $ \olds1 -> StateT $ \olds2 -> fmap swap3 $ s1s2a (olds1,olds2);

    lensStateT :: Functor m => Lens' Identity whole part -> StateT part m a -> StateT whole m a;
    lensStateT MkLens{..} (StateT pma) = StateT $ \oldw -> fmap (\(a,newp) -> (a,runIdentity $ lensPutback newp oldw)) $ pma (lensGet oldw);

    stateFst :: Functor m => StateT s m a -> StateT (s,s') m a;
    stateFst = lensStateT fstLens;

    stateSnd :: Functor m => StateT s m a -> StateT (s',s) m a;
    stateSnd = lensStateT sndLens;

    isoStateT :: Functor m => (s1 -> s2, s2 -> s1) -> StateT s1 m r -> StateT s2 m r;
    isoStateT (s1s2,s2s1) (StateT s1mrs1) = StateT $ \olds2 -> fmap (\(r,news1) -> (r,s1s2 news1)) $ s1mrs1 $ s2s1 olds2;

    traverseStateT :: (Traversable f,Applicative m) => StateT s m a -> StateT (f s) m (f a);
    traverseStateT (StateT smas) = StateT $ \oldfs -> fmap (\fas -> (fmap fst fas,fmap snd fas)) $ traverse smas oldfs;


    type StateAccess m s = forall r. StateT s m r -> m r;

    unitStateAccess :: Functor m => StateAccess m ();
    unitStateAccess = runUnitStateT;

    liftStateAccess :: Functor m => StateAccess m s -> StateAccess (StateT s1 m) s;
    liftStateAccess acc str = tunnel $ \unlift -> acc $ unlift $ swapStateT str;

    lensStateAccess :: Functor m => Lens' Identity whole part -> StateAccess m whole -> StateAccess m part;
    lensStateAccess lens sta st = sta $ lensStateT lens st;

    pairStateAccess :: Functor m => StateAccess m sa -> StateAccess m sb -> StateAccess m (sa,sb);
    pairStateAccess rsam rsbm sabr = rsam $ StateT $ \oldsa -> rsbm $ StateT $ \oldsb -> fmap swap3 $ runStateT sabr (oldsa,oldsb);

    splitStateAccess :: Monad m => StateAccess m (s1,s2) -> StateAccess (StateT s2 m) s1;
    splitStateAccess acc str = lift $ acc $ joinStateT str;
}
