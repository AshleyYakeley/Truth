module Control.Monad.Ology.Trans.ReaderState where

import Control.Monad.Ology.Data
import Control.Monad.Ology.Function
import Import

type ReaderStateT f m = StateT (WMFunction f m) m

evalReaderStateT :: Monad m => ReaderStateT f m a -> (f --> m) -> m a
evalReaderStateT rsa fm = evalStateT rsa (MkWMFunction fm)

liftRS :: (Monad f, Monad m) => f --> ReaderStateT f m
liftRS fa = do
    MkWMFunction fm <- get
    a <- lift $ fm fa
    put $ MkWMFunction $ \c -> fm $ fa >> c
    return a

updateRS :: Monad m => (f --> f) -> ReaderStateT f m ()
updateRS ff = modify (\fm -> fm . MkWMFunction ff)

rsParamRef ::
       forall f m a. Monad m
    => Param f a
    -> Ref (ReaderStateT f m) a
rsParamRef MkParam {..} = let
    getD :: ReaderStateT f m a
    getD = do
        MkWMFunction ff <- get
        lift $ ff askD
    modifyD :: (a -> a) -> ReaderStateT f m ()
    modifyD aa = updateRS $ localD aa
    in MkRef {..}
