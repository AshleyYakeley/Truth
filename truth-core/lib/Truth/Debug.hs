{-# OPTIONS -fno-warn-orphans #-}

module Truth.Debug(module Debug.ThreadTrace) where

import Truth.Core.Import
import Debug.ThreadTrace

instance MonadTransConstraint MonadIO t => TraceThing (Unlift t) where
    traceThing prefix unlift =
        MkUnlift $ \(tma :: t m a) -> case hasTransConstraint @MonadIO @t @m of
            Dict -> traceBarrier prefix (runUnlift unlift)  tma

instance MonadIO m => TraceThing (UnliftIO m) where
    traceThing prefix unlift =
        MkTransform $ traceBarrier prefix (runTransform unlift)
