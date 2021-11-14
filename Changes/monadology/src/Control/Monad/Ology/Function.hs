module Control.Monad.Ology.Function where

import Import

-- type (-->) :: forall k. (k -> Type) -> (k -> Type) -> Type
type p --> q = forall a. p a -> q a

type WMFunction :: forall k. (k -> Type) -> (k -> Type) -> Type
newtype WMFunction p q = MkWMFunction
    { runWMFunction :: p --> q
    }

wLift :: (MonadTrans t, Monad m) => WMFunction m (t m)
wLift = MkWMFunction lift

wLiftIO :: MonadIO m => WMFunction IO m
wLiftIO = MkWMFunction liftIO

instance Category WMFunction where
    id = MkWMFunction id
    (MkWMFunction bc) . (MkWMFunction ab) = MkWMFunction $ bc . ab

-- type (-/->) :: forall k. (k -> Type) -> (k -> Type) -> Type
type ma -/-> mb = forall r. ((mb --> ma) -> ma r) -> mb r

mBackFunctionToFunction :: (ma -/-> mb) -> ma --> mb
mBackFunctionToFunction mbf ma = mbf $ \_ -> ma

type WMBackFunction :: forall k. (k -> Type) -> (k -> Type) -> Type
newtype WMBackFunction p q = MkWMBackFunction
    { runWMBackFunction :: p -/-> q
    }

instance Category WMBackFunction where
    id = MkWMBackFunction $ \f -> f id
    (MkWMBackFunction bc) . (MkWMBackFunction ab) = MkWMBackFunction $ \f -> bc $ \mcmb -> ab $ \mbma -> f $ mbma . mcmb
