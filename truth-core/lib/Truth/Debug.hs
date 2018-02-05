module Truth.Debug where

import Debug.Trace
import Truth.Core.Import

contextStr :: String -> String -> String
contextStr "" b = b
contextStr a b = a ++ ": " ++ b

traceBracketArgs :: MonadIO m => String -> String -> (r -> String) -> m r -> m r
traceBracketArgs s args showr ma = do
    liftIO $ traceIO $ s ++ " [ " ++ args
    a <- ma
    liftIO $ traceIO $ s ++ " ] " ++ showr a
    return a

traceBracket :: MonadIO m => String -> m r -> m r
traceBracket s = traceBracketArgs s "" (\_ -> "")

traceUnlift :: MonadTransConstraint MonadIO t => String -> Unlift t -> Unlift t
traceUnlift name unlift =
    MkUnlift $ \tma ->
        traceBracket (contextStr name "outside") $
        runUnlift unlift $ withTransConstraintTM @MonadIO $ traceBracket (contextStr name "inside") tma
