module Truth.Debug(module Debug.ThreadTrace,module Truth.Debug) where

import Truth.Core.Import
import Debug.ThreadTrace

class TraceThing t where
    traceThing :: String -> t -> t

instance MonadTransConstraint MonadIO t => TraceThing (Unlift t) where
    traceThing prefix unlift =
        MkUnlift $ \tma ->
            traceBracket (contextStr prefix "outside") $
            runUnlift unlift $ withTransConstraintTM @MonadIO $ traceBracket (contextStr prefix "inside") tma

instance MonadIO m => TraceThing (UnliftIO m) where
    traceThing prefix unlift =
        MkTransform $ \ma ->
            traceBracket (contextStr prefix "outside") $
            runTransform unlift $ traceBracket (contextStr prefix "inside") ma

instance {-# OVERLAPPABLE #-} MonadIO m => TraceThing (m a) where
    traceThing s ma = traceBracket s ma

instance TraceThing t => TraceThing (a -> t) where
    traceThing s at a = traceThing s $ at a
