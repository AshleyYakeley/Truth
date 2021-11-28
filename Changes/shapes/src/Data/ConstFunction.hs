module Data.ConstFunction where

import Data.CatFunctor
import Shapes.Import

data ConstFunction a b
    = ConstConstFunction b
    | FunctionConstFunction (a -> b)

applyConstFunction :: ConstFunction a b -> a -> b
applyConstFunction (ConstConstFunction b) _ = b
applyConstFunction (FunctionConstFunction ab) a = ab a

applyConstFunctionA :: (Applicative m) => ConstFunction a b -> m a -> m b
applyConstFunctionA (ConstConstFunction b) _ = pure b
applyConstFunctionA (FunctionConstFunction ab) ma = fmap ab ma

instance Functor (ConstFunction t) where
    fmap ab (ConstConstFunction a) = ConstConstFunction (ab a)
    fmap ab (FunctionConstFunction ta) = FunctionConstFunction (ab . ta)

cofmap1CF :: (p -> q) -> ConstFunction q a -> ConstFunction p a
cofmap1CF _ (ConstConstFunction a) = ConstConstFunction a
cofmap1CF pq (FunctionConstFunction qa) = FunctionConstFunction (qa . pq)

instance Applicative (ConstFunction t) where
    pure = ConstConstFunction
    (ConstConstFunction ab) <*> fta = fmap ab fta
    (FunctionConstFunction tab) <*> fta = FunctionConstFunction (\t -> tab t (applyConstFunction fta t))

instance Monad (ConstFunction t) where
    (ConstConstFunction a) >>= amb = amb a
    (FunctionConstFunction ta) >>= amb = FunctionConstFunction (\t -> applyConstFunction (amb (ta t)) t)

instance Category ConstFunction where
    id = FunctionConstFunction id
    (ConstConstFunction c) . _ = ConstConstFunction c
    (FunctionConstFunction bc) . fab = fmap bc fab

instance Arrow ConstFunction where
    arr = FunctionConstFunction
    first abc = FunctionConstFunction (\(b, d) -> (applyConstFunction abc b, d))

class (Functor f) => FunctorGetPure f where
    getPure :: forall a b. ConstFunction (f a) (b -> f b)
    getPure = FunctionConstFunction (\fa b -> fmap (const b) fa)

applicativeGetPure :: (Applicative f) => ConstFunction (f a) (b -> f b)
applicativeGetPure = ConstConstFunction pure

instance FunctorGetPure ((->) p) where
    getPure = applicativeGetPure

instance (FunctorGetPure f) => CatFunctor ConstFunction ConstFunction f where
    cfmap (ConstConstFunction b) = fmap (\bfb -> bfb b) getPure
    cfmap (FunctionConstFunction ab) = FunctionConstFunction (fmap ab)

instance FunctorGetPure Identity where
    getPure = applicativeGetPure

instance FunctorGetPure Maybe where
    getPure = applicativeGetPure

instance FunctorGetPure (Either p) where
    getPure = applicativeGetPure

instance FunctorGetPure ((,) p)

instance FunctorGetPure (Result e) where
    getPure = applicativeGetPure

constFunctionAp :: (MonadOne f, Applicative (t (f a)), CatFunctor t t f) => f (t a b) -> t (f a) (f b)
constFunctionAp fcab =
    case retrieveOne fcab of
        FailureResult fn -> pure $ fmap never fn
        SuccessResult cab -> cfmap cab
