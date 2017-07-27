module Data.Knowledge where
{
    import Data.Kind;
    import Data.Type.Heterogeneous;
    import Control.Monad;
    import Control.Monad.Trans.Reader as R;


    type HetWit = forall k. k -> *;

    newtype Knowledge (m :: * -> *) (w :: HetWit) (f :: HetWit) = MkKnowledge (forall (k :: *) (a :: k). w a -> ReaderT (Knowledge m w f) m (f a));

    instance forall (m :: * -> *) (w :: HetWit) (f :: HetWit). MonadPlus m => Monoid (Knowledge m w f) where
    {
        mempty = MkKnowledge $ \_ -> mzero;
        mappend (MkKnowledge kfa) (MkKnowledge kfb) = MkKnowledge $ \w -> mplus (kfa w) (kfb w);
    };

    addKnowledge :: forall (m :: * -> *) (w :: HetWit) (f :: HetWit) (a :: *).
        MonadPlus m => Knowledge m w f -> ReaderT (Knowledge m w f) m a -> ReaderT (Knowledge m w f) m a;
    addKnowledge k = local $ \k' -> mappend k' k;

    runKnowledge :: forall (m :: * -> *) (w :: HetWit) (f :: HetWit) (a :: *).
        Knowledge m w f -> ReaderT (Knowledge m w f) m a -> m a;
    runKnowledge k r = runReaderT r k;

    getKnowledge :: forall (m :: * -> *) (w :: HetWit) (f :: HetWit).
        Monad m => ReaderT (Knowledge m w f) m (Knowledge m w f);
    getKnowledge = R.ask;

    ask :: forall (m :: * -> *) (w :: HetWit) (f :: HetWit) (k :: *) (a :: k).
        MonadPlus m => w a -> ReaderT (Knowledge m w f) m (f a);
    ask w = ReaderT $ \k@(MkKnowledge f) -> runReaderT (f w) k;

    know :: forall (m :: * -> *) (w :: HetWit) (f :: HetWit) (k :: *) (a :: k).
        (MonadPlus m,TestHetEquality w) => w a -> f a -> Knowledge m w f;
    know w a = MkKnowledge $ \w' -> case testHetEquality w w' of
    {
        Just ReflH -> return a;
        Nothing -> mzero;
    };

    knowDependent :: forall (m :: * -> *) (w :: HetWit) (f :: HetWit).
        Monad m => (Knowledge m w f -> m (Knowledge m w f)) -> Knowledge m w f;
    knowDependent ff = MkKnowledge $ \wa -> ReaderT $ \k -> do
    {
        MkKnowledge kwm <- ff k;
        runReaderT (kwm wa) k;
    };
}
