module Data.Reity.Construct where
{
    import Data.Reity.Info;
    --import Data.Maybe;
    --import Control.Monad;
    import Data.Kind;

    class (Property f) => Construct (f :: k -> *) where
    {
        construct :: forall (t :: k). f t -> Info t;
    };

    data Match (t :: kb) where
    {
        MkMatch :: forall (ka :: *) (kb :: *) (f :: ka -> kb) (a :: ka). Info f -> Info a -> Match (f a);
    };

    instance Property Match where
    {
        matchProperty (MkInfo _ (ConsWit tf ta) _) = return (MkMatch tf ta);
        matchProperty _ = Nothing;
    };

    instance Construct Match where
    {
        construct (MkMatch tf ta) = applyInfo tf ta;
    };
}
