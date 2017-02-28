module Data.Knowledge where
{
    import Data.Kind;
    import Data.Type.Equality;
    import Control.Monad;

    newtype Knowledge (w :: * -> *) = MkKnowledge (forall (a :: *). Knowledge w -> w a -> Maybe a);

    instance Monoid (Knowledge w) where
    {
        mempty = MkKnowledge (\_ _ -> mzero);
        mappend (MkKnowledge fa) (MkKnowledge fb) = MkKnowledge (\k' w -> mplus (fa k' w) (fb k' w));
    };

    ask :: Knowledge w -> w a -> Maybe a;
    ask k@(MkKnowledge f) w = f k w;

    know :: forall w a. TestEquality w => w a -> a -> Knowledge w;
    know w a = MkKnowledge (\_ w' -> do
    {
        Refl <- testEquality w w';
        return a;
    });

    knowDependent :: forall w b. (forall a. w a -> Maybe (w b,b -> a)) -> Knowledge w;
    knowDependent wamwbba = MkKnowledge (\k wa -> do
    {
        (wb,ba) <- wamwbba wa;
        b <- ask k wb;
        return (ba b);
    });
}
