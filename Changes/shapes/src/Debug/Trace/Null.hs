module Debug.Trace.Null where

import Shapes.Import

trace :: String -> a -> a
trace _ a = a

traceIO :: String -> IO ()
traceIO _ = return ()

traceM :: Applicative m => String -> m ()
traceM _ = pure ()
