module Pinafore.Language.Library.Env
    ( envLibSection
    ) where

import Import
import Pinafore.Context
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Storage ()
import Pinafore.Language.Library.Stream

getVar :: (?qcontext :: InvocationInfo) => Text -> Maybe Text
getVar n = fmap pack $ lookup (unpack n) $ iiEnvironment ?qcontext

langStdIn :: (?qcontext :: InvocationInfo) => LangSource Text
langStdIn = MkLangSource $ hoistSource liftIO $ iiStdIn ?qcontext

langStdOut :: (?qcontext :: InvocationInfo) => LangSink Text
langStdOut = MkLangSink $ hoistSink liftIO $ iiStdOut ?qcontext

langStdErr :: (?qcontext :: InvocationInfo) => LangSink Text
langStdErr = MkLangSink $ hoistSink liftIO $ iiStdErr ?qcontext

envLibSection :: LibraryStuff InvocationInfo
envLibSection =
    headingBDS "Env" "The environment in which the script was invoked." $
    pure $
    namespaceBDS
        "Env"
        [ valBDS "scriptName" "The name of the script." (pack $ iiScriptName ?qcontext :: Text)
        , valBDS "arguments" "Arguments passed to the script." (fmap pack $ iiScriptArguments ?qcontext :: [Text])
        , valBDS
              "variables"
              "Environment variables."
              (fmap (\(n, v) -> (pack n, pack v)) $ iiEnvironment ?qcontext :: [(Text, Text)])
        , valBDS "getVar" "Get environment variable." getVar
        , valBDS "stdin" "Standard input source." langStdIn
        , valBDS "stdout" "Standard output sink." langStdOut
        , valBDS "stderr" "Standard error/diagnostics sink." langStdErr
        , valBDS "outputLn" "Output text and a newline to standard output. Same as `writeLn stdout`." $
          langSinkWriteLn langStdOut
        ]
