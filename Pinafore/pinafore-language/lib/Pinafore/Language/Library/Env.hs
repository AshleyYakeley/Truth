module Pinafore.Language.Library.Env
    ( envStuff
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Context
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Library.Storage ()
import Pinafore.Language.Library.Stream
import Shapes

getVar :: (?qcontext :: InvocationInfo) => Text -> Maybe Text
getVar n = fmap pack $ lookup (unpack n) $ iiEnvironment ?qcontext

langStdIn :: (?qcontext :: InvocationInfo) => LangSource Text
langStdIn = MkLangSource $ hoistSource liftIO $ iiStdIn ?qcontext

langStdOut :: (?qcontext :: InvocationInfo) => LangSink Text
langStdOut = MkLangSink $ hoistSink liftIO $ iiStdOut ?qcontext

langStdErr :: (?qcontext :: InvocationInfo) => LangSink Text
langStdErr = MkLangSink $ hoistSink liftIO $ iiStdErr ?qcontext

openDefaultStore :: (?qcontext :: InvocationInfo) => View QStore
openDefaultStore = do
    model <- iiDefaultStorageModel ?qcontext
    liftIO $ mkQStore model

envStuff :: BindDocTree InvocationInfo
envStuff =
    headingBDT "Env" "The environment in which the script was invoked." $
    pure $
    namespaceBDT
        "Env"
        ""
        [ valBDT "scriptName" "The name of the script." (pack $ iiScriptName ?qcontext :: Text)
        , valBDT "arguments" "Arguments passed to the script." (fmap pack $ iiScriptArguments ?qcontext :: [Text])
        , valBDT
              "variables"
              "Environment variables."
              (fmap (\(n, v) -> (pack n, pack v)) $ iiEnvironment ?qcontext :: [(Text, Text)])
        , valBDT "getVar" "Get environment variable." getVar
        , valBDT "stdin" "Standard input source." langStdIn
        , valBDT "stdout" "Standard output sink." langStdOut
        , valBDT "stderr" "Standard error/diagnostics sink." langStdErr
        , valBDT "outputLn" "Output text and a newline to standard output. Same as `writeLn stdout`." $
          langSinkWriteLn langStdOut
        , valBDT
              "openDefaultStore"
              "Open the default `Store`. Will be closed at the end of the lifecycle."
              openDefaultStore
        ]
