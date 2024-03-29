module Pinafore.Main
    ( ModuleOptions(..)
    , standardFetchModule
    , nullInvocationInfo
    , StorageModelOptions(..)
    , standardStorageModel
    , sqliteQDumpTable
    , qInterpretTextAtType
    , qInterpretText
    , qInterpretFile
    , qInteractHandles
    , qInteract
    ) where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Context
import Pinafore.Language
import Pinafore.Storage
import Shapes
import System.FilePath

data StorageModelOptions = MkStorageModelOptions
    { smoCache :: Bool
    , smoDataDir :: FilePath
    }

data ModuleOptions = MkModuleOptions
    { moExtraLibrary :: [LibraryModule ()]
    , moModuleDirs :: [FilePath]
    }

standardStorageModel :: StorageModelOptions -> View (Model QStorageUpdate)
standardStorageModel MkStorageModelOptions {..} = do
    rc <- viewGetResourceContext
    viewLiftLifecycle $ do
        sqlReference <- liftIO $ sqliteTableReference $ smoDataDir </> "tables.sqlite3"
        tableReference1 <- exclusiveResource rc sqlReference
        tableReference <-
            if smoCache
                then do
                    tableReferenceF <- cacheReference rc 500000 tableReference1 -- half-second delay before writing
                    return $ tableReferenceF rc
                else return tableReference1
        (model, ()) <- makeSharedModel $ reflectingPremodel $ qTableEntityReference tableReference
        return model

standardFetchModule :: forall context. ModuleOptions -> FetchModule context
standardFetchModule MkModuleOptions {..} = let
    extraLibFetchModule :: FetchModule context
    extraLibFetchModule = contramap (\_ -> ()) $ libraryFetchModule moExtraLibrary
    dirFetchModule :: FetchModule context
    dirFetchModule = mconcat $ fmap directoryFetchModule moModuleDirs
    in extraLibFetchModule <> dirFetchModule

sqliteQDumpTable :: FilePath -> IO ()
sqliteQDumpTable dirpath = do
    MkAllFor tables <- sqliteTableGetEntireDatabase emptyResourceContext $ dirpath </> "tables.sqlite3"
    let
        littable :: [(Entity, Literal)]
        littable = fmap (\(MkAllOf lrow) -> (lrow LiteralKey, lrow LiteralValue)) $ tables $ MkTupleTableSel QSLiteral
    for_ (tables $ MkTupleTableSel QSProperty) $ \(MkAllOf row) -> let
        p = row TriplePredicate
        s = row TripleSubject
        v = row TripleValue
        lv =
            case lookup v littable of
                Just l -> show l
                Nothing -> show v
        in putStrLn $ show p ++ " " ++ show s ++ " = " ++ lv

qInterpretTextAtType ::
       forall t m. (?library :: LibraryContext, HasQType 'Negative t, MonadIO m, MonadThrow QError m)
    => FilePath
    -> Text
    -> m t
qInterpretTextAtType puipath puitext = fromInterpretResult $ runPinaforeScoped puipath $ parseValueUnify puitext

qInterpretText :: (?library :: LibraryContext, MonadIO m, MonadThrow QError m) => FilePath -> Text -> m (View ())
qInterpretText puipath puitext = do
    action <- qInterpretTextAtType @(Action TopType) puipath puitext
    return $ runAction $ fmap (\MkTopType -> ()) $ action

qInterpretFile :: (?library :: LibraryContext) => FilePath -> View (View ())
qInterpretFile fpath = do
    ptext <- liftIO $ readFile fpath
    qInterpretText fpath $ decodeUtf8 $ toStrict ptext

qInteractHandles :: (?library :: LibraryContext) => Handle -> Handle -> Bool -> View ()
qInteractHandles inh outh echo = interact inh outh echo

qInteract :: (?library :: LibraryContext) => View ()
qInteract = qInteractHandles stdin stdout False
