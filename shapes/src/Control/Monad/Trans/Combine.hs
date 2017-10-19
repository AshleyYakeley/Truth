module Control.Monad.Trans.Combine where

import Control.Monad.Trans.Constraint
import Control.Monad.Trans.Tunnel
import Control.Monad.Trans.Unlift
import Shapes.Import

type UnliftIO m = forall r. m r -> IO r

remonadUnliftIO :: MonadTransTunnel t => (forall a. m1 a -> m2 a) -> UnliftIO (t m2) -> UnliftIO (t m1)
remonadUnliftIO ff r2 m1a = r2 $ remonad ff m1a

class MonadUnliftIO m =>
      MonadCombineIO m where
    type CombineMonadIO m (m' :: * -> *) :: * -> *
    isCombineMonadIO_ ::
           forall m' proxya proxyb. MonadCombineIO m'
        => proxya m
        -> proxyb m'
        -> Dict (MonadCombineIO (CombineMonadIO m m'))
    combineLiftFst_ ::
           forall m' r proxy. MonadCombineIO m'
        => proxy m'
        -> m r
        -> CombineMonadIO m m' r
    combineLiftSnd_ ::
           forall m' r proxy. MonadCombineIO m'
        => proxy m
        -> m' r
        -> CombineMonadIO m m' r
    combineUnliftIOs :: forall m'. UnliftIO m -> UnliftIO m' -> UnliftIO (CombineMonadIO m m')

isCombineMonadIO ::
       forall ma mb. (MonadCombineIO ma, MonadCombineIO mb)
    => Dict (MonadCombineIO (CombineMonadIO ma mb))
isCombineMonadIO = isCombineMonadIO_ (Proxy :: Proxy ma) (Proxy :: Proxy mb)

combineLiftFst ::
       forall ma mb r. (MonadCombineIO ma, MonadCombineIO mb)
    => ma r
    -> CombineMonadIO ma mb r
combineLiftFst = combineLiftFst_ (Proxy :: Proxy mb)

combineLiftSnd ::
       forall ma mb r. (MonadCombineIO ma, MonadCombineIO mb)
    => mb r
    -> CombineMonadIO ma mb r
combineLiftSnd = combineLiftSnd_ (Proxy :: Proxy ma)

instance MonadCombineIO IO where
    type CombineMonadIO IO m' = m'
    isCombineMonadIO_ _ _ = Dict
    combineLiftFst_ _ = liftIO
    combineLiftSnd_ _ = id
    combineUnliftIOs _ r' = r'

instance (MonadTransUnlift t, MonadCombineIO m, MonadIO (t m)) => MonadCombineIO (t m) where
    type CombineMonadIO (t m) m' = t (CombineMonadIO m m')
    isCombineMonadIO_ _ (pm' :: proxy m') =
        case isCombineMonadIO_ (Proxy :: Proxy m) pm' of
            Dict ->
                case hasTransConstraint @MonadIO @t @(CombineMonadIO m m') of
                    Dict -> Dict
    combineLiftFst_ pm' tmr = tunnel $ \unlift -> combineLiftFst_ @m pm' $ unlift tmr
    combineLiftSnd_ _ (m'r :: m' r) =
        case isCombineMonadIO @m @m' of
            Dict -> lift $ combineLiftSnd @m m'r
    combineUnliftIOs rtm rm' ctm =
        rtm $ tunnel $ \tmrma -> liftIOWithUnlift $ \rm -> combineUnliftIOs rm rm' $ tmrma ctm

instance MonadTransUnlift t => MonadTransConstraint MonadCombineIO t where
    hasTransConstraint ::
           forall m. MonadCombineIO m
        => Dict (MonadCombineIO (t m))
    hasTransConstraint =
        case hasTransConstraint @MonadIO @t @m of
            Dict -> Dict
