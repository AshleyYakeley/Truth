module Data.Knowledge where
{
    import Data.Kind;
    import Data.Type.Heterogeneous;
    import Control.Monad;


    type HetWit = forall k. k -> *;

    newtype Knowledge (m :: * -> *) (w :: HetWit) (f :: HetWit) = MkKnowledge (forall (k :: *) (a :: k). Knowledge m w f -> w a -> m (f a));

    instance forall (m :: * -> *) (w :: HetWit) (f :: HetWit). MonadPlus m => Monoid (Knowledge m w f) where
    {
        mempty = MkKnowledge $ \_ _ -> mzero;
        mappend (MkKnowledge kfa) (MkKnowledge kfb) = MkKnowledge $ \k' w -> mplus (kfa k' w) (kfb k' w);
    };

    ask :: forall (m :: * -> *) (w :: HetWit) (f :: HetWit) (k :: *) (a :: k). MonadPlus m => Knowledge m w f -> w a -> m (f a);
    ask k@(MkKnowledge f) w = f k w;

    know :: forall (m :: * -> *) (w :: HetWit) (f :: HetWit) (k :: *) (a :: k). (MonadPlus m,TestHetEquality w) => w a -> f a -> Knowledge m w f;
    know w a = MkKnowledge $ \_ w' -> case testHetEquality w w' of
    {
        Just ReflH -> return a;
        Nothing -> mzero;
    };
}
