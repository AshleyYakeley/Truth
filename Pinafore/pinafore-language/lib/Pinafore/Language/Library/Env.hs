module Pinafore.Language.Library.Env
    ( envLibraryModule
    ) where

import Pinafore.Context
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Library.Stream
import Shapes

getVar :: (?pinafore :: PinaforeContext) => Text -> Maybe Text
getVar n = fmap pack $ lookup (unpack n) $ iiEnvironment pinaforeInvocationInfo

langStdIn :: (?pinafore :: PinaforeContext) => LangSource Text
langStdIn = MkLangSource $ hoistSource liftIO $ iiStdIn pinaforeInvocationInfo

langStdOut :: (?pinafore :: PinaforeContext) => LangSink Text
langStdOut = MkLangSink $ hoistSink liftIO $ iiStdOut pinaforeInvocationInfo

langStdErr :: (?pinafore :: PinaforeContext) => LangSink Text
langStdErr = MkLangSink $ hoistSink liftIO $ iiStdErr pinaforeInvocationInfo

envLibraryModule :: LibraryModule
envLibraryModule =
    MkDocTree
        "Env"
        "The environment in which the script was invoked."
        [ mkValEntry "scriptName" "The name of the script." (pack $ iiScriptName pinaforeInvocationInfo :: Text)
        , mkValEntry
              "arguments"
              "Arguments passed to the script."
              (fmap pack $ iiScriptArguments pinaforeInvocationInfo :: [Text])
        , mkValEntry
              "variables"
              "Environment variables."
              (fmap (\(n, v) -> (pack n, pack v)) $ iiEnvironment pinaforeInvocationInfo :: [(Text, Text)])
        , mkValEntry "getVar" "Get environment variable." getVar
        , mkValEntry "stdin" "Standard input source." langStdIn
        , mkValEntry "stdout" "Standard output sink." langStdOut
        , mkValEntry "stderr" "Standard error/diagnostics sink." langStdErr
        , mkValEntry "outputLn" "Output text and a newline to standard output. Same as `writeLn stdout`." $
          langSinkWriteLn langStdOut
        ]
