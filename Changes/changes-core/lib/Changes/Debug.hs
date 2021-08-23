{-# OPTIONS -fno-warn-orphans #-}

module Changes.Debug(module Debug.ThreadTrace, module Changes.Debug) where

import Changes.Core.Import
import Debug.ThreadTrace

traceUnliftAll :: forall t m. (MonadTransConstraint MonadIO t, MonadIO m) => String -> MFunction (t m) m -> MFunction (t m) m
traceUnliftAll prefix mf = case hasTransConstraint @MonadIO @t @m of
    Dict -> traceBarrier prefix mf

instance (forall m. c m => MonadIO m, MonadTransConstraint MonadIO t) => TraceThing (WUnliftAll c t) where
    traceThing prefix unlift =
        MkWUnliftAll $ traceUnliftAll prefix (runWUnliftAll unlift)

instance MonadIO m => TraceThing (WIOFunction m) where
    traceThing prefix unlift =
        MkWMFunction $ traceBarrier prefix (runWMFunction unlift)
