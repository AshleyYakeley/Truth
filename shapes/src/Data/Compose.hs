{-# OPTIONS -fno-warn-orphans #-}
module Data.Compose where
{
    import Shapes.Import;


    deriving instance Semigroup (p (q a)) => Semigroup (Compose p q a);
    deriving instance Monoid (p (q a)) => Monoid (Compose p q a);

    instance (Monad p,Monad q,Traversable q) => Monad (Compose p q) where
    {
        return = pure;
        (Compose pqa) >>= f = Compose $ do
        {
            qa <- pqa;
            qqb <- traverse (getCompose . f) qa;
            return $ qqb >>= id;
        };
    };

    instance (MonadIO p,Monad q,Traversable q) => MonadIO (Compose p q) where
    {
        liftIO ioa = Compose $ liftIO $ fmap pure ioa;
    };
}
