module Data.Knowledge where
{
    import Data.Kind;
    import Data.Type.Heterogeneous;
    import Control.Monad;


    type HetWit = forall k. k -> *;

    newtype Knowledge (w :: HetWit) (f :: HetWit) = MkKnowledge (forall (k :: *) (a :: k). Knowledge w f -> w a -> Maybe (f a));

    instance forall (w :: HetWit) (f :: HetWit). Monoid (Knowledge w f) where
    {
        mempty = MkKnowledge $ \_ _ -> mzero;
        mappend (MkKnowledge kfa) (MkKnowledge kfb) = MkKnowledge $ \k' w -> mplus (kfa k' w) (kfb k' w);
    };

    ask :: forall (w :: HetWit) (f :: HetWit) (k :: *) (a :: k). Knowledge w f -> w a -> Maybe (f a);
    ask k@(MkKnowledge f) w = f k w;

    know :: forall (w :: HetWit) (f :: HetWit) (k :: *) (a :: k). TestHetEquality w => w a -> f a -> Knowledge w f;
    know w a = MkKnowledge $ \_ w' -> do
    {
        ReflH <- testHetEquality w w';
        return a;
    };
}
