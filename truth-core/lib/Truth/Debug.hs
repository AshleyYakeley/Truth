{-# OPTIONS -fno-warn-orphans #-}

module Truth.Debug(module Debug.ThreadTrace) where

import Truth.Core.Import
import Debug.ThreadTrace

instance (forall m. c m => MonadIO m, MonadTransConstraint MonadIO t) => TraceThing (WUnliftAll c t) where
    traceThing prefix unlift =
        MkWUnliftAll $ \(tma :: t m a) -> case hasTransConstraint @MonadIO @t @m of
            Dict -> traceBarrier prefix (runWUnliftAll unlift)  tma

instance MonadIO m => TraceThing (WIOFunction m) where
    traceThing prefix unlift =
        MkWMFunction $ traceBarrier prefix (runWMFunction unlift)
