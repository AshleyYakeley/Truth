module Debug.Trace.Null where

import Prelude

trace :: String -> a -> a
trace _ a = a

traceIO :: String -> IO ()
traceIO _ = return ()

traceM :: Applicative m => String -> m ()
traceM _ = pure ()
