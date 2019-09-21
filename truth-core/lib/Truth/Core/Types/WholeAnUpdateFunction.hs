module Truth.Core.Types.WholeAnUpdateFunction where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Whole

newtype WholeAnUpdateFunction tr update a = MkWholeAnUpdateFunction
    { unWholeAnUpdateFunction :: AnUpdateFunction tr update (WholeUpdate a)
    }

instance MonadTransConstraint MonadIO tr => Functor (WholeAnUpdateFunction tr update) where
    fmap :: forall a b. (a -> b) -> WholeAnUpdateFunction tr update a -> WholeAnUpdateFunction tr update b
    fmap ab (MkWholeAnUpdateFunction (MkAnUpdateFunction g u)) = let
        g' :: forall m t. MonadIO m
           => MutableRead m (UpdateReader update)
           -> WholeReader b t
           -> tr m t
        g' mr ReadWhole = withTransConstraintTM @MonadIO $ fmap ab $ g mr ReadWhole
        u' :: forall m. MonadIO m
           => update
           -> MutableRead m (UpdateReader update)
           -> tr m [WholeUpdate b]
        u' update mr =
            withTransConstraintTM @MonadIO $
            fmap (fmap $ \(MkWholeReaderUpdate a) -> MkWholeReaderUpdate $ ab a) $ u update mr
        in MkWholeAnUpdateFunction $ MkAnUpdateFunction g' u'

instance MonadTransConstraint MonadIO tr => Applicative (WholeAnUpdateFunction tr update) where
    pure :: forall a. a -> WholeAnUpdateFunction tr update a
    pure a = let
        g' :: forall m t. MonadIO m
           => MutableRead m (UpdateReader update)
           -> WholeReader a t
           -> tr m t
        g' _mr ReadWhole = withTransConstraintTM @MonadIO $ return a
        u' :: forall m. MonadIO m
           => update
           -> MutableRead m (UpdateReader update)
           -> tr m [WholeUpdate a]
        u' _update _mr = withTransConstraintTM @MonadIO $ return []
        in MkWholeAnUpdateFunction $ MkAnUpdateFunction g' u'
    p <*> q = p >>= \ab -> q >>= \a -> pure (ab a)

instance MonadTransConstraint MonadIO tr => IsoVariant (WholeAnUpdateFunction tr update)

instance MonadTransConstraint MonadIO tr => Monad (WholeAnUpdateFunction tr update) where
    (>>=) ::
           forall a b.
           WholeAnUpdateFunction tr update a
        -> (a -> WholeAnUpdateFunction tr update b)
        -> WholeAnUpdateFunction tr update b
    MkWholeAnUpdateFunction (MkAnUpdateFunction g u) >>= f = let
        g' :: forall m t. MonadIO m
           => MutableRead m (UpdateReader update)
           -> WholeReader b t
           -> tr m t
        g' mr ReadWhole =
            withTransConstraintTM @MonadIO $ do
                a <- g mr ReadWhole
                ufGet (unWholeAnUpdateFunction $ f a) mr ReadWhole
        u' :: forall m. MonadIO m
           => update
           -> MutableRead m (UpdateReader update)
           -> tr m [WholeUpdate b]
        u' update mr =
            withTransConstraintTM @MonadIO $ do
                updates <- u update mr
                case lastWholeUpdate updates of
                    Just a -> ufUpdate (unWholeAnUpdateFunction $ f a) update mr
                    Nothing -> return []
        in MkWholeAnUpdateFunction $ MkAnUpdateFunction g' u'

instance MonadTransConstraint MonadIO tr => MonadIO (WholeAnUpdateFunction tr update) where
    liftIO :: forall a. IO a -> WholeAnUpdateFunction tr update a
    liftIO ioa = let
        g' :: forall m t. MonadIO m
           => MutableRead m (UpdateReader update)
           -> WholeReader a t
           -> tr m t
        g' _mr ReadWhole = withTransConstraintTM @MonadIO $ liftIO ioa
        u' :: forall m. MonadIO m
           => update
           -> MutableRead m (UpdateReader update)
           -> tr m [WholeUpdate a]
        u' _update _mr = withTransConstraintTM @MonadIO $ return []
        in MkWholeAnUpdateFunction $ MkAnUpdateFunction g' u'
