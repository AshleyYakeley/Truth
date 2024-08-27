module Pinafore.Main
    ( processorCountRef
    , ProcessorCount(..)
    , ExecutionOptions(..)
    , defaultExecutionOptions
    , RunWithOptions(..)
    , ModuleOptions(..)
    , module Pinafore.Context
    , standardLibraryContext
    , pinaforeLibrary
    , sqliteQDumpTable
    , qInterpretTextAtType
    , qInterpretScriptText
    , qInterpretScriptFile
    , qInteractHandles
    , qInteract
    ) where

import GHC.Conc
import Import
import Pinafore.Context
import Pinafore.Language
import Pinafore.Language.Expression
import Pinafore.Language.Type
import Pinafore.Storage
import System.FilePath

class RunWithOptions a where
    runWithOptions :: a -> IO --> IO

instance RunWithOptions () where
    runWithOptions () = id

instance RunWithOptions a => RunWithOptions (Maybe a) where
    runWithOptions Nothing = id
    runWithOptions (Just a) = runWithOptions a

processorCountRef :: Ref IO Int
processorCountRef = MkRef getNumCapabilities setNumCapabilities

data ProcessorCount
    = SpecificProcessorCount Int
    | AllProcessorCount

instance RunWithOptions ProcessorCount where
    runWithOptions pc mr = do
        nc <-
            case pc of
                AllProcessorCount -> getNumProcessors
                SpecificProcessorCount i -> return i
        refPutRestore processorCountRef nc mr

data ExecutionOptions = MkExecutionOptions
    { eoProcessorCount :: Maybe ProcessorCount
    }

instance RunWithOptions ExecutionOptions where
    runWithOptions MkExecutionOptions {..} = runWithOptions eoProcessorCount

defaultProcessorCountINTERNAL :: Maybe ProcessorCount
defaultProcessorCountINTERNAL = Nothing

defaultExecutionOptions :: ExecutionOptions
defaultExecutionOptions = MkExecutionOptions {eoProcessorCount = defaultProcessorCountINTERNAL}

data ModuleOptions = MkModuleOptions
    { moLibraryModules :: [LibraryModule]
    , moModuleDirs :: [FilePath]
    }

standardLoadModule :: ModuleOptions -> LoadModule
standardLoadModule MkModuleOptions {..} = let
    libLoadModule :: LoadModule
    libLoadModule = libraryLoadModule moLibraryModules
    dirLoadModule :: LoadModule
    dirLoadModule = mconcat $ fmap directoryLoadModule moModuleDirs
    in libLoadModule <> dirLoadModule

standardLibraryContext :: ModuleOptions -> LibraryContext
standardLibraryContext modopts = mkLibraryContext $ standardLoadModule modopts

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
       forall t m. (?library :: LibraryContext, HasQType QPolyShim 'Negative t, MonadIO m, MonadThrow QError m)
    => FilePath
    -> Text
    -> [String]
    -> [(ImplicitName, QValue)]
    -> m t
qInterpretTextAtType puipath puitext args impargs = let
    arglist = qToValue @(NonEmpty Text) $ fmap pack $ puipath :| args
    in fromInterpretResult $
       runPinaforeScoped puipath $ parseToValueUnify puitext $ (MkImplicitName "arglist", arglist) : impargs

qInterpretScriptText ::
       (?library :: LibraryContext, MonadIO m, MonadThrow QError m)
    => FilePath
    -> Text
    -> [String]
    -> [(ImplicitName, QValue)]
    -> m (View ())
qInterpretScriptText puipath puitext args impargs = do
    action <- qInterpretTextAtType @(Action TopType) puipath puitext args impargs
    return $ runAction $ fmap (\MkTopType -> ()) $ action

qInterpretScriptFile ::
       (?library :: LibraryContext) => FilePath -> [String] -> [(ImplicitName, QValue)] -> View (View ())
qInterpretScriptFile fpath args impargs = do
    ptext <- liftIO $ readFile fpath
    qInterpretScriptText fpath (decodeUtf8 $ toStrict ptext) args impargs

qInteractHandles :: (?library :: LibraryContext) => Handle -> Handle -> Bool -> View ()
qInteractHandles inh outh echo = interact inh outh echo

qInteract :: (?library :: LibraryContext) => View ()
qInteract = qInteractHandles stdin stdout False
