module Pinafore.Language.Debug where

import Shapes

debugMessage :: Text -> IO ()
debugMessage t = hPutStrLn stderr $ unpack t
-- debugMessage t = traceIOM $ unpack t
