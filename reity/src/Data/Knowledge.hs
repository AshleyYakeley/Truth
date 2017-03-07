module Data.Knowledge where
{
    import Data.Kind;
    import Data.Type.Equality;
    import Control.Monad;

    newtype Knowledge (w :: k -> *) (f :: k -> *) = MkKnowledge (forall (a :: k). Knowledge w f -> w a -> Maybe (f a));

    instance Monoid (Knowledge w f) where
    {
        mempty = MkKnowledge $ \_ _ -> mzero;
        mappend (MkKnowledge kfa) (MkKnowledge kfb) = MkKnowledge $ \k' w -> mplus (kfa k' w) (kfb k' w);
    };

    ask :: Knowledge w f -> w a -> Maybe (f a);
    ask k@(MkKnowledge f) w = f k w;

    know :: forall w f a. TestEquality w => w a -> f a -> Knowledge w f;
    know w a = MkKnowledge $ \_ w' -> do
    {
        Refl <- testEquality w w';
        return a;
    };
}
