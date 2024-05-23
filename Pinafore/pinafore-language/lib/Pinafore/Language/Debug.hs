module Pinafore.Language.Debug where

import Import

debugMessage :: Text -> IO ()
debugMessage t = hPutStrLn stderr $ unpack t
