module Pinafore.Language.Library.Std.Invocation
    ( invocationLibEntries
    ) where

import Pinafore.Context
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Shapes

getEnv :: (?pinafore :: PinaforeContext) => Text -> Maybe Text
getEnv n = fmap pack $ lookup (unpack n) $ iiEnvironment pinaforeInvocationInfo

invocationLibEntries :: [DocTreeEntry BindDoc]
invocationLibEntries =
    [ docTreeEntry
          "Invocation"
          "How the script was invoked."
          [ mkValEntry "scriptName" "The name of the script." (pack $ iiScriptName pinaforeInvocationInfo :: Text)
          , mkValEntry
                "scriptArguments"
                "Arguments passed to the script."
                (fmap pack $ iiScriptArguments pinaforeInvocationInfo :: [Text])
          , mkValEntry
                "environment"
                "Environment variables."
                (fmap (\(n, v) -> (pack n, pack v)) $ iiEnvironment pinaforeInvocationInfo :: [(Text, Text)])
          , mkValEntry "getEnv" "Get environment variable." getEnv
          ]
    ]
