module Control.Monad.Ology.Data.Exn where

import Control.Monad.Ology.General
import Control.Monad.Ology.Specific.Result
import Import

type Exn :: (Type -> Type) -> Type -> Type
data Exn m e = MkExn
    { throwD :: forall a. e -> m a
    , catchD :: forall a. m a -> (e -> m a) -> m a
    , maskD :: m -/-> m
    }

tryD :: Monad m => Exn m e -> m a -> m (Result e a)
tryD exn ma = catchD exn (fmap SuccessResult ma) $ \e -> return $ FailureResult e

handleD :: Exn m e -> (e -> m a) -> m a -> m a
handleD exn handler ma = catchD exn ma handler

onExceptionD ::
       forall e m a. Monad m
    => Exn m e
    -> m a
    -> m ()
    -> m a
onExceptionD exn ma handler = catchD exn ma $ \e -> handler >> throwD exn e

bracketD ::
       forall e m a b. Monad m
    => Exn m e
    -> m a
    -> (a -> m ())
    -> (a -> m b)
    -> m b
bracketD exn before after thing =
    maskD exn $ \restore -> do
        a <- before
        r <- onExceptionD exn (restore (thing a)) (after a)
        _ <- after a
        return r

finallyD ::
       forall e m a. Monad m
    => Exn m e
    -> m a
    -> m ()
    -> m a
finallyD exn ma handler = bracketD exn (return ()) (const handler) (const ma)

bracket_D ::
       forall e m. Monad m
    => Exn m e
    -> m ()
    -> m ()
    -> m --> m
bracket_D exn before after thing = bracketD exn before (const after) (const thing)

mapExn :: (e2 -> e1) -> (e1 -> Maybe e2) -> Exn m e1 -> Exn m e2
mapExn f g exn =
    MkExn
        { throwD = throwD exn . f
        , catchD =
              \ma handler ->
                  catchD exn ma $ \e ->
                      case g e of
                          Nothing -> throwD exn e
                          Just e' -> handler e'
        , maskD = maskD exn
        }

liftExn ::
       forall e t m. (MonadTransTunnel t, Monad m)
    => Exn m e
    -> Exn (t m) e
liftExn (MkExn t c m) = let
    t' :: forall a. e -> t m a
    t' e = lift $ t e
    c' :: forall a. t m a -> (e -> t m a) -> t m a
    c' tma handler = tunnel $ \unlift -> c (unlift tma) $ \e -> unlift $ handler e
    m' :: t m -/-> t m
    m' = backHoist m
    in MkExn t' c' m'

allExn ::
       forall m. MonadException m
    => Exn m (Exc m)
allExn = MkExn throwExc catchExc $ runWMBackFunction id

allIOExn ::
       forall m. (MonadException m, MonadTunnelIO m)
    => Exn m (Exc m)
allIOExn = MkExn throwExc catchExc mask

someExn ::
       forall e m. MonadCatch e m
    => Exn m e
someExn = MkExn throw catch $ runWMBackFunction id

someIOExn ::
       forall e m. (MonadCatch e m, MonadTunnelIO m)
    => Exn m e
someIOExn = MkExn throw catch mask
