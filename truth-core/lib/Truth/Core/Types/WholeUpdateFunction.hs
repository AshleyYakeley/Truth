module Truth.Core.Types.WholeUpdateFunction where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Whole

newtype WholeUpdateFunction update a = MkWholeUpdateFunction
    { unWholeUpdateFunction :: UpdateFunction update (WholeUpdate a)
    }

instance Functor (WholeUpdateFunction update) where
    fmap :: forall a b. (a -> b) -> WholeUpdateFunction update a -> WholeUpdateFunction update b
    fmap ab (MkWholeUpdateFunction (MkUpdateFunction g u)) = let
        g' :: forall m t. MonadIO m
           => MutableRead m (UpdateReader update)
           -> WholeReader b t
           -> m t
        g' mr ReadWhole = fmap ab $ g mr ReadWhole
        u' :: forall m. MonadIO m
           => update
           -> MutableRead m (UpdateReader update)
           -> m [WholeUpdate b]
        u' update mr = fmap (fmap $ \(MkWholeReaderUpdate a) -> MkWholeReaderUpdate $ ab a) $ u update mr
        in MkWholeUpdateFunction $ MkUpdateFunction g' u'

instance Applicative (WholeUpdateFunction update) where
    pure :: forall a. a -> WholeUpdateFunction update a
    pure a = let
        g' :: forall m t. MonadIO m
           => MutableRead m (UpdateReader update)
           -> WholeReader a t
           -> m t
        g' _mr ReadWhole = return a
        u' :: forall m. MonadIO m
           => update
           -> MutableRead m (UpdateReader update)
           -> m [WholeUpdate a]
        u' _update _mr = return []
        in MkWholeUpdateFunction $ MkUpdateFunction g' u'
    p <*> q = p >>= \ab -> q >>= \a -> pure (ab a)

instance IsoVariant (WholeUpdateFunction update)

instance Monad (WholeUpdateFunction update) where
    (>>=) ::
           forall a b.
           WholeUpdateFunction update a
        -> (a -> WholeUpdateFunction update b)
        -> WholeUpdateFunction update b
    MkWholeUpdateFunction (MkUpdateFunction g u) >>= f = let
        g' :: forall m t. MonadIO m
           => MutableRead m (UpdateReader update)
           -> WholeReader b t
           -> m t
        g' mr ReadWhole = do
            a <- g mr ReadWhole
            ufGet (unWholeUpdateFunction $ f a) mr ReadWhole
        u' :: forall m. MonadIO m
           => update
           -> MutableRead m (UpdateReader update)
           -> m [WholeUpdate b]
        u' update mr = do
            updates <- u update mr
            case lastWholeUpdate updates of
                Just a -> ufUpdate (unWholeUpdateFunction $ f a) update mr
                Nothing -> return []
        in MkWholeUpdateFunction $ MkUpdateFunction g' u'

instance MonadIO (WholeUpdateFunction update) where
    liftIO :: forall a. IO a -> WholeUpdateFunction update a
    liftIO ioa = let
        g' :: forall m t. MonadIO m
           => MutableRead m (UpdateReader update)
           -> WholeReader a t
           -> m t
        g' _mr ReadWhole = liftIO ioa
        u' :: forall m. MonadIO m
           => update
           -> MutableRead m (UpdateReader update)
           -> m [WholeUpdate a]
        u' _update _mr = return []
        in MkWholeUpdateFunction $ MkUpdateFunction g' u'
