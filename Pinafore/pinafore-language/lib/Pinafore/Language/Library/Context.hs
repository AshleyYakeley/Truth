module Pinafore.Language.Library.Context
    ( contextLibraryModule
    ) where

import Pinafore.Context
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Library.Stream
import Shapes

getEnv :: (?pinafore :: PinaforeContext) => Text -> Maybe Text
getEnv n = fmap pack $ lookup (unpack n) $ iiEnvironment pinaforeInvocationInfo

langStdIn :: LangSource Text
langStdIn = MkLangSource $ hoistSource liftIO stdinTextSource

langStdOut :: LangSink Text
langStdOut = MkLangSink $ hoistSink liftIO stdoutTextSink

langStdErr :: LangSink Text
langStdErr = MkLangSink $ hoistSink liftIO stderrTextSink

contextLibraryModule :: LibraryModule
contextLibraryModule =
    MkDocTree
        "Context"
        "How the script was invoked."
        [ mkValEntry "scriptName" "The name of the script." (pack $ iiScriptName pinaforeInvocationInfo :: Text)
        , mkValEntry
              "arguments"
              "Arguments passed to the script."
              (fmap pack $ iiScriptArguments pinaforeInvocationInfo :: [Text])
        , mkValEntry
              "environment"
              "Environment variables."
              (fmap (\(n, v) -> (pack n, pack v)) $ iiEnvironment pinaforeInvocationInfo :: [(Text, Text)])
        , mkValEntry "getEnv" "Get environment variable." getEnv
        , mkValEntry "stdin" "Standard input source." langStdIn
        , mkValEntry "stdout" "Standard output sink." langStdOut
        , mkValEntry "stderr" "Standard error/diagnostics sink." langStdErr
        ]
