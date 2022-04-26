module Control.Monad.Ology.Data.Exn where

import Control.Monad.Ology.General
import Import
import Control.Monad.Ology.Specific.Result

data Exn m e = MkExn
    { throwD :: forall a. e -> m a
    , catchD :: forall a. m a -> (e -> m a) -> m a
    }

tryD :: Monad m => Exn m e -> m a -> m (Result e a)
tryD exn ma = catchD exn (fmap SuccessResult ma) $ \e -> return $ FailureResult e

handleD ::
       Exn m e -> (e -> m a)
    -> m a
    -> m a
handleD exn handler ma = catchD exn ma handler

mapExn :: (e2 -> e1) -> (e1 -> Maybe e2) -> Exn m e1 -> Exn m e2
mapExn f g exn = MkExn
    { throwD = throwD exn . f
    , catchD = \ma handler -> catchD exn ma $ \e -> case g e of
        Nothing -> throwD exn e
        Just e' -> handler e'
    }

excExn :: MonadException m => Exn m (Exc m)
excExn = MkExn throwExc catchExc

monadExn :: MonadCatch e m => Exn m e
monadExn = MkExn throw catch
