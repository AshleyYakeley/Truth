module Pinafore.Language.Library.Env
    ( envLibraryModule
    ) where

import Pinafore.Context
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Library.Stream
import Pinafore.Language.Name
import Shapes

getVar :: (?qcontext :: QContext) => Text -> Maybe Text
getVar n = fmap pack $ lookup (unpack n) $ iiEnvironment qInvocationInfo

langStdIn :: (?qcontext :: QContext) => LangSource Text
langStdIn = MkLangSource $ hoistSource liftIO $ iiStdIn qInvocationInfo

langStdOut :: (?qcontext :: QContext) => LangSink Text
langStdOut = MkLangSink $ hoistSink liftIO $ iiStdOut qInvocationInfo

langStdErr :: (?qcontext :: QContext) => LangSink Text
langStdErr = MkLangSink $ hoistSink liftIO $ iiStdErr qInvocationInfo

envLibraryModule :: LibraryModule
envLibraryModule =
    MkDocTree "Env" "The environment in which the script was invoked." $
    namespaceRelative
        "Env"
        [ mkValEntry "scriptName" "The name of the script." (pack $ iiScriptName qInvocationInfo :: Text)
        , mkValEntry
              "arguments"
              "Arguments passed to the script."
              (fmap pack $ iiScriptArguments qInvocationInfo :: [Text])
        , mkValEntry
              "variables"
              "Environment variables."
              (fmap (\(n, v) -> (pack n, pack v)) $ iiEnvironment qInvocationInfo :: [(Text, Text)])
        , mkValEntry "getVar" "Get environment variable." getVar
        , mkValEntry "stdin" "Standard input source." langStdIn
        , mkValEntry "stdout" "Standard output sink." langStdOut
        , mkValEntry "stderr" "Standard error/diagnostics sink." langStdErr
        , mkValEntry "outputLn" "Output text and a newline to standard output. Same as `writeLn stdout`." $
          langSinkWriteLn langStdOut
        ]
