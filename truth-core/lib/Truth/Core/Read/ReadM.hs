module Truth.Core.Read.ReadM where

import Truth.Core.Import
import Truth.Core.Read.Readable

newtype ReadM r a = MkReadM
    { unReadM :: forall m. MonadIO m => Readable m r -> m a
    }

instance Functor (ReadM r) where
    fmap ab (MkReadM rma) = MkReadM $ \r -> fmap ab $ rma r

instance Applicative (ReadM r) where
    pure a = MkReadM $ \_ -> pure a
    MkReadM rmab <*> MkReadM rma = MkReadM $ \r -> rmab r <*> rma r

instance Monad (ReadM r) where
    return = pure
    MkReadM rma >>= f = MkReadM $ \r -> rma r >>= \a -> unReadM (f a) r

instance MonadIO (ReadM r) where
    liftIO ioa = MkReadM $ \_ -> liftIO ioa

mapReadM :: ReadFunction ra rb -> ReadM rb t -> ReadM ra t
mapReadM rf (MkReadM rmt) = MkReadM $ \r -> rmt $ rf r

readM :: r t -> ReadM r t -- = Readable (Read r) r
readM rt = MkReadM $ \rd -> rd rt

readableToReadFunction :: Readable (ReadM ra) rb -> ReadFunction ra rb
readableToReadFunction mrr mra rt = unReadM (mrr rt) mra

readFunctionToReadable :: ReadFunction ra rb -> Readable (ReadM ra) rb
readFunctionToReadable rf rt = MkReadM $ \rb -> rf rb rt
