module Pinafore.Test
    ( moduleScopeEntries
    , checkUpdateEditor
    , QTableSubject(..)
    , makeTestStorage
    , TesterOptions(..)
    , defaultTester
    , LoadModule(..)
    , testerLoad
    , testerLoadLibrary
    , Tester
    , runTester
    , testerLiftView
    , testerLiftAction
    , testerRunAction
    , testerLiftInterpreter
    , testerInterpret
    , testerInterpretScriptFile
    , testerGetStore
    , testerGetTableState
    , libraryLoadModule
    , directoryLoadModule
    , lcLoadModule
    , qInterpretScriptFile
    , qInterpretTextAtType
    ) where

import Import
import Pinafore.Language
import Pinafore.Language.Expression
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Stream
import Pinafore.Language.Type
import Pinafore.Main
import Pinafore.Storage

moduleScopeEntries :: QModule -> [(FullName, QBindingInfo)]
moduleScopeEntries qmod = bindingMapEntries $ scopeBindings $ moduleScope qmod

checkUpdateEditor ::
       forall a. Eq a
    => a
    -> View ()
    -> Editor (WholeUpdate a) ()
checkUpdateEditor val push =
    MkEditor $ \_ -> do
        var <- liftIO newEmptyMVar
        let
            editingUpdate :: NonEmpty (WholeUpdate a) -> EditContext -> View ()
            editingUpdate updates _ = liftIO $ putMVar var updates
            editingDo :: Task IO () -> View ()
            editingDo _ = do
                push
                updates <- liftIO $ takeMVar var
                case updates of
                    MkWholeReaderUpdate v :| []
                        | v == val -> return ()
                    _ -> fail "unexpected push"
            editingTask = mempty
        return MkEditing {..}

makeTestStorage :: Lifecycle (Model QStorageUpdate, View QTableSubject)
makeTestStorage = do
    tableStateReference :: Reference (WholeEdit QTableSubject) <-
        liftIO $ makeMemoryReference (MkQTableSubject [] [] [] []) $ \_ -> True
    let
        tableReference :: Reference QTableEdit
        tableReference = convertReference tableStateReference
        getTableState :: View QTableSubject
        getTableState = do
            rc <- viewGetResourceContext
            liftIO $ getReferenceSubject rc tableStateReference
    (model, ()) <- makeSharedModel $ reflectingPremodel $ qTableEntityReference tableReference
    return (model, getTableState)

data TesterOptions = MkTesterOptions
    { tstExecutionOptions :: ExecutionOptions
    , tstOutput :: Handle
    , tstLibrary :: [LibraryModule]
    }

defaultTester :: TesterOptions
defaultTester = let
    tstExecutionOptions = defaultExecutionOptions
    tstOutput = stdout
    tstLibrary = pinaforeLibrary
    in MkTesterOptions {..}

data TesterContext = MkTesterContext
    { tcStorageModel :: Model QStorageUpdate
    , tcLibrary :: LibraryContext
    , tcGetTableState :: View QTableSubject
    }

newtype Tester a = MkTester
    { unTester :: ReaderT TesterContext View a
    } deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadException, MonadFix, MonadHoistIO, MonadTunnelIO)

instance MonadUnliftIO Tester where
    liftIOWithUnlift call = MkTester $ liftIOWithUnlift $ \unlift -> call $ unlift . unTester

overrideLibraryModule :: ModuleName -> (LibraryStuff -> LibraryStuff) -> [LibraryModule] -> [LibraryModule]
overrideLibraryModule mname f =
    fmap $ \case
        MkLibraryModule n c
            | n == mname -> MkLibraryModule n $ f c
        lm -> lm

runTester :: TesterOptions -> Tester () -> IO ()
runTester MkTesterOptions {..} (MkTester ta) =
    runWithOptions tstExecutionOptions $
    runLifecycle $ do
        (tcStorageModel, tcGetTableState) <- makeTestStorage
        let
            outputSink :: Sink Action Text
            outputSink = hoistSink liftIO $ handleSinkText tstOutput
            testOutputLn :: Text -> Action ()
            testOutputLn = sinkWriteLn outputSink
            myLibStuff :: LibraryStuff
            myLibStuff =
                namespaceBDS
                    "Env"
                    [valBDS "stdout" "OVERRIDDEN" $ MkLangSink outputSink, valBDS "outputLn" "OVERRIDDEN" testOutputLn]
            tcLibrary =
                mkLibraryContext $
                libraryLoadModule $ overrideLibraryModule builtInModuleName (\lib -> lib <> myLibStuff) tstLibrary
        runView $ runReaderT ta $ MkTesterContext {..}

contextParam :: Param Tester TesterContext
contextParam = MkParam (MkTester ask) $ \a (MkTester m) -> MkTester $ with a m

testerLoad :: LoadModule -> Tester --> Tester
testerLoad lm =
    paramLocal contextParam $ \tc ->
        tc
            { tcLibrary =
                  let
                      tcl = tcLibrary tc
                      in tcl {lcLoadModule = lcLoadModule tcl <> lm}
            }

testerLoadLibrary :: [LibraryModule] -> Tester --> Tester
testerLoadLibrary lms = testerLoad $ libraryLoadModule lms

testerLiftView :: forall a. ((?library :: LibraryContext) => View a) -> Tester a
testerLiftView va =
    MkTester $
    ReaderT $ \MkTesterContext {..} -> let
        ?library = tcLibrary
        in va

testerRunAction :: Action () -> Tester ()
testerRunAction pa = testerLiftView $ runAction pa

testerLiftAction :: Action --> Tester
testerLiftAction pa = testerLiftView $ unliftActionOrFail pa

testerLiftInterpreterPath :: forall a. FilePath -> ((?library :: LibraryContext) => QInterpreter a) -> Tester a
testerLiftInterpreterPath fpath pia = testerLiftView $ fromInterpretResult $ runPinaforeScoped fpath pia

testerLiftInterpreter :: forall a. ((?library :: LibraryContext) => QInterpreter a) -> Tester a
testerLiftInterpreter = testerLiftInterpreterPath "<input>"

testerGetStore :: Tester QStore
testerGetStore = do
    model <- MkTester $ asks tcStorageModel
    liftIO $ mkQStore model

testerGetTableState :: Tester QTableSubject
testerGetTableState =
    MkTester $ do
        gts <- asks tcGetTableState
        lift gts

testerGetImplications :: Tester [(ImplicitName, QValue)]
testerGetImplications = do
    store <- testerGetStore
    let
        getStore :: Action QStore
        getStore = return store
    return [(MkImplicitName "openTestStore", qToValue getStore)]

testerInterpret ::
       forall a. HasQType QPolyShim 'Negative a
    => Text
    -> Tester a
testerInterpret script = do
    impls <- testerGetImplications
    testerLiftInterpreter $ parseToValueUnify script impls

testerInterpretScriptFile :: FilePath -> [String] -> Tester (View ())
testerInterpretScriptFile fpath args = do
    impls <- testerGetImplications
    testerLiftView $ qInterpretScriptFile fpath args impls
