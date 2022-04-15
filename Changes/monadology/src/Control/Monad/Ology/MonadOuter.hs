module Control.Monad.Ology.MonadOuter where

import Import

type Extract :: (Type -> Type) -> Type
newtype Extract m = MkExtract
    { runExtract :: forall a. m a -> a
    }

-- | Instances of this type are isomorphic to @P -> a@ for some type @P@.
-- Must satisfy @fmap (\ex -> runExtract ex ma) getExtract = ma@.
class Monad m => MonadOuter m where
    getExtract :: m (Extract m)

instance MonadOuter Identity where
    getExtract = return $ MkExtract runIdentity

instance MonadOuter ((->) r) where
    getExtract r = MkExtract $ \ra -> ra r

instance MonadOuter m => MonadOuter (IdentityT m) where
    getExtract =
        IdentityT $ do
            MkExtract maa <- getExtract
            return $ MkExtract $ maa . runIdentityT

instance MonadOuter m => MonadOuter (ReaderT r m) where
    getExtract =
        ReaderT $ \r -> do
            MkExtract maa <- getExtract
            return $ MkExtract $ \(ReaderT rma) -> maa $ rma r

commuteOuter ::
       forall m f a. (MonadOuter m, Functor f)
    => f (m a)
    -> m (f a)
commuteOuter fma = do
    MkExtract ext <- getExtract
    return $ fmap ext fma
