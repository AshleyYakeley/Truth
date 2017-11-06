module Main
    ( main
    ) where

import qualified Options.Applicative as O
import Pinafore
import Shapes
import System.Environment
import Truth.Core

optParser :: O.Parser (FilePath, [FilePath])
optParser = (,) <$> (O.strOption (O.long "db")) <*> (O.many $ O.strArgument mempty)

doFile :: FilePath -> FilePath -> String -> IO ()
doFile dbpath fpath str =
    case parseValue fpath str of
        FailureResult e -> fail e
        SuccessResult qval ->
            let object :: Object (WholeEdit (FiniteSet Text))
                object = mapObject (readOnlyGeneralLens (qdisplay qval)) (sqlitePinaforeObject dbpath)
            in runObject object $ \muted -> do
                   items <- mutableRead muted ReadWhole
                   for_ items $ \item -> liftIO $ putStrLn $ unpack item

main :: IO ()
main = do
    args <- getArgs
    (dbpath, fpaths) <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args
    case fpaths of
        [] -> do
            str <- getContents
            doFile dbpath "<stdin>" str
        _ ->
            for_ fpaths $ \fpath -> do
                str <- readFile fpath
                doFile dbpath fpath str
