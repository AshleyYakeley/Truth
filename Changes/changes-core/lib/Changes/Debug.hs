{-# OPTIONS -fno-warn-orphans #-}

module Changes.Debug
    ( module Debug.ThreadTrace
    , module Changes.Debug
    )
where

import Debug.ThreadTrace

import Changes.Core.Import

traceUnlift ::
    forall t m.
    (TransConstraint MonadIO t, MonadIO m) =>
    String ->
    t m --> m ->
    t m --> m
traceUnlift prefix mf =
    case hasTransConstraint @MonadIO @t @m of
        Dict -> traceBarrier_ prefix mf

instance (forall m. c m => MonadIO m, TransConstraint MonadIO t) => TraceThing (WUnlift c t) where
    traceThing prefix unlift = MkWUnlift $ traceUnlift prefix (unWUnlift unlift)
