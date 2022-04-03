{-# OPTIONS -fno-warn-orphans #-}

module Changes.Debug(module Debug.ThreadTrace, module Changes.Debug) where

import Changes.Core.Import
import Debug.ThreadTrace

traceUnliftAll :: forall t m. (TransConstraint MonadIO t, MonadIO m) => String -> t m --> m -> t m --> m
traceUnliftAll prefix mf = case hasTransConstraint @MonadIO @t @m of
    Dict -> traceBarrier prefix mf

instance (forall m. c m => MonadIO m, TransConstraint MonadIO t) => TraceThing (WUnlift c t) where
    traceThing prefix unlift =
        MkWUnlift $ traceUnliftAll prefix (runWUnlift unlift)
