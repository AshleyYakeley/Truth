module Truth.Core.Types.WholeAnUpdateFunction where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Whole

newtype WholeAnUpdateFunction tt update a = MkWholeAnUpdateFunction
    { unWholeAnUpdateFunction :: AnUpdateFunction tt update (WholeUpdate a)
    }

instance MonadTransStackUnliftAll tt => Functor (WholeAnUpdateFunction tt update) where
    fmap :: forall a b. (a -> b) -> WholeAnUpdateFunction tt update a -> WholeAnUpdateFunction tt update b
    fmap ab (MkWholeAnUpdateFunction (MkAnUpdateFunction g u)) = let
        g' :: forall m t. MonadIO m
           => MutableRead m (UpdateReader update)
           -> WholeReader b t
           -> ApplyStack tt m t
        g' mr ReadWhole =
            case transStackDict @MonadIO @tt @m of
                Dict -> fmap ab $ g mr ReadWhole
        u' :: forall m. MonadIO m
           => update
           -> MutableRead m (UpdateReader update)
           -> ApplyStack tt m [WholeUpdate b]
        u' update mr =
            case transStackDict @MonadIO @tt @m of
                Dict -> fmap (fmap $ \(MkWholeReaderUpdate a) -> MkWholeReaderUpdate $ ab a) $ u update mr
        in MkWholeAnUpdateFunction $ MkAnUpdateFunction g' u'

instance MonadTransStackUnliftAll tt => Applicative (WholeAnUpdateFunction tt update) where
    pure :: forall a. a -> WholeAnUpdateFunction tt update a
    pure a = let
        g' :: forall m t. MonadIO m
           => MutableRead m (UpdateReader update)
           -> WholeReader a t
           -> ApplyStack tt m t
        g' _mr ReadWhole =
            case transStackDict @MonadIO @tt @m of
                Dict -> return a
        u' :: forall m. MonadIO m
           => update
           -> MutableRead m (UpdateReader update)
           -> ApplyStack tt m [WholeUpdate a]
        u' _update _mr =
            case transStackDict @MonadIO @tt @m of
                Dict -> return []
        in MkWholeAnUpdateFunction $ MkAnUpdateFunction g' u'
    p <*> q = p >>= \ab -> q >>= \a -> pure (ab a)

instance MonadTransStackUnliftAll tt => IsoVariant (WholeAnUpdateFunction tt update)

instance MonadTransStackUnliftAll tt => Monad (WholeAnUpdateFunction tt update) where
    (>>=) ::
           forall a b.
           WholeAnUpdateFunction tt update a
        -> (a -> WholeAnUpdateFunction tt update b)
        -> WholeAnUpdateFunction tt update b
    MkWholeAnUpdateFunction (MkAnUpdateFunction g u) >>= f = let
        g' :: forall m t. MonadIO m
           => MutableRead m (UpdateReader update)
           -> WholeReader b t
           -> ApplyStack tt m t
        g' mr ReadWhole =
            case transStackDict @MonadIO @tt @m of
                Dict -> do
                    a <- g mr ReadWhole
                    ufGet (unWholeAnUpdateFunction $ f a) mr ReadWhole
        u' :: forall m. MonadIO m
           => update
           -> MutableRead m (UpdateReader update)
           -> ApplyStack tt m [WholeUpdate b]
        u' update mr =
            case transStackDict @MonadIO @tt @m of
                Dict -> do
                    updates <- u update mr
                    case lastWholeUpdate updates of
                        Just a -> ufUpdate (unWholeAnUpdateFunction $ f a) update mr
                        Nothing -> return []
        in MkWholeAnUpdateFunction $ MkAnUpdateFunction g' u'

instance MonadTransStackUnliftAll tt => MonadIO (WholeAnUpdateFunction tt update) where
    liftIO :: forall a. IO a -> WholeAnUpdateFunction tt update a
    liftIO ioa = let
        g' :: forall m t. MonadIO m
           => MutableRead m (UpdateReader update)
           -> WholeReader a t
           -> ApplyStack tt m t
        g' _mr ReadWhole =
            case transStackDict @MonadIO @tt @m of
                Dict -> liftIO ioa
        u' :: forall m. MonadIO m
           => update
           -> MutableRead m (UpdateReader update)
           -> ApplyStack tt m [WholeUpdate a]
        u' _update _mr =
            case transStackDict @MonadIO @tt @m of
                Dict -> return []
        in MkWholeAnUpdateFunction $ MkAnUpdateFunction g' u'
