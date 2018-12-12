module Main
    ( main
    ) where

import qualified Options.Applicative as O
import Pinafore
import Shapes
import System.Directory
import System.Environment
import System.Environment.XDG.BaseDir
import System.FilePath hiding ((<.>))
import Truth.Core
import Truth.World.SQLite

data Options =
    ConvertOption (Maybe FilePath)
                  (Maybe FilePath)

optParser :: O.Parser Options
optParser =
    ConvertOption <$> (O.optional $ O.strOption $ O.long "in" <> O.metavar "PATH") <*>
    (O.optional $ O.strOption $ O.long "out" <> O.metavar "PATH")

getDirPath :: MonadIO m => Maybe FilePath -> m FilePath
getDirPath mdirpath = do
    dirpath <-
        case mdirpath of
            Just dirpath -> return dirpath
            Nothing -> liftIO $ getUserDataDir "pinafore"
    liftIO $ createDirectoryIfMissing True dirpath
    return dirpath

main :: IO ()
main = do
    args <- getArgs
    ConvertOption mindir moutdir <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args
    indir <- getDirPath mindir
    MkAllF tables <- sqlitePinaforeTableGetEntireDatabase $ indir </> "tables.sqlite3"
    let
        litrows = tables $ MkTupleTableSel PinaforeLiteral
        convert :: AllValue LiteralTable -> Maybe (Entity, Entity)
        convert (MkAllValue lrow) = let
            lv = lrow LiteralValue
            foundkey = lrow LiteralKey
            correctkey = literalToEntity lv
            in if foundkey /= correctkey
                   then Just (foundkey, correctkey)
                   else Nothing
        pairs = catMaybes $ fmap convert litrows
        convMap :: StrictMap Entity Entity
        convMap = mapFromList pairs
        convEntity :: Entity -> Entity
        convEntity e = fromMaybe e $ lookup e convMap
    putStrLn $ indir <> ": " <> show (length pairs) <> " bad literals out of " <> show (length litrows)
    case moutdir of
        Just outdir ->
            case sqliteObject (outdir </> "tables.sqlite3") sqlitePinaforeSchema of
                MkObject outRun _outRead outEdit -> do
                    putStrLn "Creating DB"
                    runTransform outRun $ do
                        liftIO $ putStrLn "Converting literals"
                        pushEdit $
                            outEdit $
                            fmap
                                (\(MkAllValue lrow) -> let
                                     val = lrow LiteralValue
                                     in DatabaseInsert (MkTupleTableSel PinaforeLiteral) $
                                        MkTupleInsertClause $
                                        pure $
                                        MkAllValue $ \case
                                            LiteralValue -> val
                                            LiteralKey -> literalToEntity val)
                                litrows
                        liftIO $ putStrLn "Converting triples"
                        pushEdit $
                            outEdit $
                            fmap
                                (\(MkAllValue row) ->
                                     DatabaseInsert (MkTupleTableSel PinaforeTriple) $
                                     MkTupleInsertClause $
                                     pure $
                                     MkAllValue $ \case
                                         TriplePredicate -> row TriplePredicate
                                         TripleSubject -> convEntity $ row TripleSubject
                                         TripleValue -> convEntity $ row TripleValue) $
                            tables $ MkTupleTableSel PinaforeTriple
                        liftIO $ putStrLn "Closing DB"
                    putStrLn "Done."
        Nothing -> return ()
