module Truth.Debug where

import Debug.Trace
import Truth.Core.Import

contextStr :: String -> String -> String
contextStr "" b = b
contextStr a b = a ++ ": " ++ b

traceIOM :: MonadIO m => String -> m ()
traceIOM s = liftIO $ traceIO $ s

traceBracketArgs :: MonadIO m => String -> String -> (r -> String) -> m r -> m r
traceBracketArgs s args showr ma = do
    traceIOM $ s ++ " [" ++ (if null args then "" else " " ++ args)
    a <- ma
    let
        ret = showr a
    traceIOM $ s ++ " ]" ++ (if null ret then "" else " => " ++ ret)
    return a

traceBracket :: MonadIO m => String -> m r -> m r
traceBracket s = traceBracketArgs s "" (\_ -> "")

traceUnlift :: MonadTransConstraint MonadIO t => String -> Unlift t -> Unlift t
traceUnlift name unlift =
    MkUnlift $ \tma ->
        traceBracket (contextStr name "outside") $
        runUnlift unlift $ withTransConstraintTM @MonadIO $ traceBracket (contextStr name "inside") tma
