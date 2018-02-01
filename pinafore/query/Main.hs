module Main
    ( main
    ) where

import qualified Options.Applicative as O
import Pinafore
import Shapes
import System.Environment
import Truth.Core

data Options
    = ExprDocOption
    | RunOption FilePath
                [FilePath]

optParser :: O.Parser Options
optParser =
    (O.flag' ExprDocOption $ O.long "doc") <|>
    RunOption <$> (O.strOption (O.long "db")) <*> (O.many $ O.strArgument mempty)

doFile :: FilePath -> FilePath -> String -> IO ()
doFile dbpath fpath str =
    case parseValue fpath str of
        FailureResult e -> fail e
        SuccessResult qval ->
            case mapObject (readOnlyEditLens (qdisplay qval)) (sqlitePinaforeObject dbpath) :: Object (WholeEdit (FiniteSet Text)) of
                MkObject (MkUnliftIO run) rd _ ->
                    run $ do
                        items <- rd ReadWhole
                        for_ items $ \item -> liftIO $ putStrLn $ unpack item

main :: IO ()
main = do
    args <- getArgs
    options <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args
    case options of
        ExprDocOption -> for_ predefinedDoc $ \(name, desc) -> putStrLn $ name ++ " ::\n    " ++ desc
        RunOption dbpath fpaths ->
            case fpaths of
                [] -> do
                    str <- getContents
                    doFile dbpath "<stdin>" str
                _ ->
                    for_ fpaths $ \fpath -> do
                        str <- readFile fpath
                        doFile dbpath fpath str
