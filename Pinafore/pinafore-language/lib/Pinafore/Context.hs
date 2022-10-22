module Pinafore.Context where

import Changes.Core
import Pinafore.Base
import Shapes

data InvocationInfo = MkInvocationInfo
    { iiScriptName :: String
    , iiScriptArguments :: [String]
    , iiEnvironment :: [(String, String)]
    , iiStdIn :: Source IO Text
    , iiStdOut :: Sink IO Text
    , iiStdErr :: Sink IO Text
    , iiDefaultStorageModel :: View (Model QStorageUpdate)
    }

nullInvocationInfo :: InvocationInfo
nullInvocationInfo = let
    iiScriptName = "<null>"
    iiScriptArguments = []
    iiEnvironment = []
    iiStdIn = nullSource
    iiStdOut = mempty
    iiStdErr = mempty
    iiDefaultStorageModel = return $ error "no default storage model"
    in MkInvocationInfo {..}
