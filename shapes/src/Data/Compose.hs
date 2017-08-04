module Data.Compose where
{
    import Prelude;
    import Data.Kind;
    import Control.Monad.IO.Class;


    data Compose (p :: k2 -> *) (q :: k1 -> k2) (a :: k1) = MkCompose {getCompose :: p (q a)};

    instance (Functor p,Functor q) => Functor (Compose p q) where
    {
        fmap ab (MkCompose pqa) = MkCompose (fmap (fmap ab) pqa);
    };

    instance (Applicative p,Applicative q) => Applicative (Compose p q) where
    {
        pure a = MkCompose (pure (pure a));
        (MkCompose pqab) <*> (MkCompose pqa) = MkCompose ((fmap (<*>) pqab) <*> pqa);
    };

    instance (Monad p,Monad q,Traversable q) => Monad (Compose p q) where
    {
        return = pure;
        (MkCompose pqa) >>= f = MkCompose $ do
        {
            qa <- pqa;
            qqb <- traverse (getCompose . f) qa;
            return $ qqb >>= id;
        };
    };

    instance (Foldable p,Foldable q) => Foldable (Compose p q) where
    {
        foldMap am (MkCompose pqa) = foldMap (foldMap am) pqa;
    };

    instance (Traversable p,Traversable q) => Traversable (Compose p q) where
    {
        sequenceA (MkCompose pqfa) = fmap MkCompose $ sequenceA $ fmap sequenceA pqfa
    };

    instance (MonadIO p,Monad q,Traversable q) => MonadIO (Compose p q) where
    {
        liftIO ioa = MkCompose $ liftIO $ fmap pure ioa;
    };
}
