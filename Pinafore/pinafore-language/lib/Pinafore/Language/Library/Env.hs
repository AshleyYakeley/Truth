module Pinafore.Language.Library.Env
    ( envLibraryModule
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Context
import Pinafore.Language.DocTree
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

envLibraryModule :: LibraryModule InvocationInfo
envLibraryModule =
    MkLibraryModule $
    MkDocTree
        "Env"
        "The environment in which the script was invoked."
        [ mkValEntry "scriptName" "The name of the script." (pack $ iiScriptName ?qcontext :: Text)
        , mkValEntry "arguments" "Arguments passed to the script." (fmap pack $ iiScriptArguments ?qcontext :: [Text])
        , mkValEntry
              "variables"
              "Environment variables."
              (fmap (\(n, v) -> (pack n, pack v)) $ iiEnvironment ?qcontext :: [(Text, Text)])
        , mkValEntry "getVar" "Get environment variable." getVar
        , mkValEntry "stdin" "Standard input source." langStdIn
        , mkValEntry "stdout" "Standard output sink." langStdOut
        , mkValEntry "stderr" "Standard error/diagnostics sink." langStdErr
        , mkValEntry "outputLn" "Output text and a newline to standard output. Same as `writeLn stdout`." $
          langSinkWriteLn langStdOut
        , mkValEntry
              "openDefaultStore"
              "Open the default `Store`. Will be closed at the end of the lifecycle."
              openDefaultStore
        ]
