module Pinafore.Language.Library.Env
    ( envLibSection
    ) where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Storage ()
import Pinafore.Language.Library.Stream
import Pinafore.Language.Type
import Pinafore.Language.VarID
import System.Environment (lookupEnv)
import System.Environment.Blank

getVar :: Text -> IO (Maybe Text)
getVar n = fmap (fmap pack) $ lookupEnv $ unpack n

setVar :: Text -> Maybe Text -> IO ()
setVar n (Just v) = setEnv (unpack n) (unpack v) True
setVar n Nothing = unsetEnv (unpack n)

getVariables :: IO [(Text, Text)]
getVariables = do
    vars <- getEnvironment
    return $ fmap (\(n, v) -> (pack n, pack v)) vars

langStdIn :: LangSource Text
langStdIn = MkLangSource $ hoistSource liftIO stdinTextSource

langStdOut :: LangSink Text
langStdOut = MkLangSink $ hoistSink liftIO stdoutTextSink

langStdErr :: LangSink Text
langStdErr = MkLangSink $ hoistSink liftIO stderrTextSink

type ArgList = NonEmpty Text

scriptName :: ArgList -> Text
scriptName (n :| _) = n

argListImplicitVar :: NameWitness VarID (QShimWit 'Negative) ArgList
argListImplicitVar = MkNameWitness (ImplicitVarID $ MkImplicitName "arglist") qType

scriptNameExpr :: QExpression
scriptNameExpr = MkSealedExpression qType $ OpenExpression argListImplicitVar $ ClosedExpression scriptName

arguments :: ArgList -> [Text]
arguments (_ :| args) = args

argumentsExpr :: QExpression
argumentsExpr = MkSealedExpression qType $ OpenExpression argListImplicitVar $ ClosedExpression arguments

envLibSection :: LibraryStuff
envLibSection =
    headingBDS "Env" "The environment in which the script was invoked." $
    pure $
    namespaceBDS
        "Env"
        [ valExprBDS "scriptName" "The name of the script." scriptNameExpr
        , valExprBDS "arguments" "Arguments passed to the script." argumentsExpr
        , valBDS "getVariables" "Environment variables." getVariables
        , valBDS "getVariable" "Get environment variable." getVar
        , valBDS "setVariable" "Set (or unset) environment variable. Note this is not thread-safe." setVar
        , valBDS "stdin" "Standard input source." langStdIn
        , valBDS "stdout" "Standard output sink." langStdOut
        , valBDS "stderr" "Standard error/diagnostics sink." langStdErr
        , valBDS "outputLn" "Output text and a newline to standard output. Same as `writeLn stdout`." $
          langSinkWriteLn langStdOut
        ]
