module Control.Monad.Ology.Specific.CoroutineT where

import Control.Monad.Ology.General.IO
import Control.Monad.Ology.General.Trans.Constraint
import Control.Monad.Ology.General.Trans.Hoist
import Control.Monad.Ology.General.Trans.Trans
import Import

newtype CoroutineT p q m a = MkCoroutineT
    { resumeCoroutine :: m (Either a (p, q -> CoroutineT p q m a))
    }

instance Functor m => Functor (CoroutineT p q m) where
    fmap ab (MkCoroutineT ma) = MkCoroutineT $ fmap (bimap ab $ fmap $ fmap $ fmap ab) ma

instance TransConstraint Functor (CoroutineT p q) where
    hasTransConstraint = Dict

instance Monad m => Applicative (CoroutineT p q m) where
    pure a = MkCoroutineT $ pure $ Left a
    mab <*> ma = do
        ab <- mab
        a <- ma
        return $ ab a

instance Monad m => Monad (CoroutineT p q m) where
    return = pure
    MkCoroutineT mea >>= f =
        MkCoroutineT $ do
            ea <- mea
            case ea of
                Left a -> resumeCoroutine $ f a
                Right (p, qf) -> return $ Right $ (p, \q -> qf q >>= f)

instance TransConstraint Monad (CoroutineT p q) where
    hasTransConstraint = Dict

instance MonadIO m => MonadIO (CoroutineT p q m) where
    liftIO ioa = lift $ liftIO ioa

instance TransConstraint MonadIO (CoroutineT p q) where
    hasTransConstraint = Dict

instance MonadTrans (CoroutineT p q) where
    lift ma = MkCoroutineT $ fmap Left ma

instance MonadTransHoist (CoroutineT p q) where
    hoist f (MkCoroutineT ma) = MkCoroutineT $ (fmap $ fmap $ fmap $ fmap $ hoist f) $ f ma

yieldCoroutine :: Monad m => p -> CoroutineT p q m q
yieldCoroutine p = MkCoroutineT $ return $ Right (p, return)

joinCoroutines :: Monad m => CoroutineT q r m a -> (q -> CoroutineT p q m a) -> CoroutineT p r m a
joinCoroutines cqr qcpq =
    MkCoroutineT $ do
        eqra <- resumeCoroutine cqr
        case eqra of
            Left a -> return $ Left a
            Right (q, rf) -> do
                epqa <- resumeCoroutine $ qcpq q
                return $ (fmap $ fmap $ \qf r -> joinCoroutines (rf r) qf) epqa

runCoroutine :: Monad m => CoroutineT p p m a -> m a
runCoroutine susp = do
    eap <- resumeCoroutine susp
    case eap of
        Left a -> return a
        Right (p, psusp) -> runCoroutine $ psusp p
