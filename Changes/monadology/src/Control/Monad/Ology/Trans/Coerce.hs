module Control.Monad.Ology.Trans.Coerce where

import Import

class MonadTrans t => MonadTransCoerce t where
    transCoerce ::
           forall m1 m2. Coercible m1 m2
        => Dict (Coercible (t m1) (t m2))

instance MonadTransCoerce IdentityT where
    transCoerce = Dict

instance MonadTransCoerce (ReaderT r) where
    transCoerce = Dict

instance Monoid w => MonadTransCoerce (WriterT w) where
    transCoerce = Dict

instance MonadTransCoerce (StateT a) where
    transCoerce = Dict

instance MonadTransCoerce MaybeT where
    transCoerce = Dict

instance MonadTransCoerce (ExceptT e) where
    transCoerce = Dict

instance MonadTransCoerce (ContT r) where
    transCoerce = Dict
