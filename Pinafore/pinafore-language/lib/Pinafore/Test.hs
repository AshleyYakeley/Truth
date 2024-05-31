module Pinafore.Test
    ( moduleScopeEntries
    , checkUpdateEditor
    , QTableSubject(..)
    , makeTestInvocationInfo
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
    , testerGetDefaultStore
    , testerGetTableState
    , libraryLoadModule
    , directoryLoadModule
    , lcLoadModule
    , qInterpretFile
    , qInterpretTextAtType
    ) where

import Import
import Pinafore.Context
import Pinafore.Language
import Pinafore.Language.Interpreter
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

makeTestInvocationInfo :: Handle -> Lifecycle (InvocationInfo, View QTableSubject)
makeTestInvocationInfo hout = do
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
    let ii = nullInvocationInfo {iiStdOut = handleSinkText hout, iiDefaultStorageModel = return model}
    return (ii, getTableState)

data TesterOptions = MkTesterOptions
    { tstOutput :: Handle
    }

defaultTester :: TesterOptions
defaultTester = let
    tstOutput = stdout
    in MkTesterOptions {..}

data TesterContext = MkTesterContext
    { tcInvocationInfo :: InvocationInfo
    , tcLibrary :: LibraryContext
    , tcGetTableState :: View QTableSubject
    }

newtype Tester a = MkTester
    { unTester :: ReaderT TesterContext View a
    } deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadException, MonadFix, MonadHoistIO, MonadTunnelIO)

instance MonadUnliftIO Tester where
    liftIOWithUnlift call = MkTester $ liftIOWithUnlift $ \unlift -> call $ unlift . unTester

runTester :: TesterOptions -> Tester () -> IO ()
runTester MkTesterOptions {..} (MkTester ta) =
    runLifecycle $ do
        (ii, getTableState) <- makeTestInvocationInfo tstOutput
        let library = mkLibraryContext ii mempty
        runView $ runReaderT ta $ MkTesterContext ii library getTableState

contextParam :: Param Tester TesterContext
contextParam = MkParam (MkTester ask) $ \a (MkTester m) -> MkTester $ with a m

testerWithLoadModule :: (InvocationInfo -> LoadModule) -> Tester --> Tester
testerWithLoadModule iifm =
    paramLocal contextParam $ \tc ->
        tc
            { tcLibrary =
                  let
                      tcl = tcLibrary tc
                      in tcl {lcLoadModule = lcLoadModule tcl <> iifm (tcInvocationInfo tc)}
            }

testerLoad :: LoadModule -> Tester --> Tester
testerLoad lm = testerWithLoadModule $ \_ -> lm

testerLoadLibrary :: [LibraryModule ()] -> Tester --> Tester
testerLoadLibrary lms = testerLoad $ libraryLoadModule () lms

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

testerLiftInterpreter :: forall a. ((?library :: LibraryContext) => QInterpreter a) -> Tester a
testerLiftInterpreter pia = testerLiftView $ fromInterpretResult $ runPinaforeScoped "<input>" pia

testerGetDefaultStore :: Tester QStore
testerGetDefaultStore = do
    ii <- MkTester $ asks tcInvocationInfo
    model <- testerLiftView $ iiDefaultStorageModel ii
    liftIO $ mkQStore model

testerGetTableState :: Tester QTableSubject
testerGetTableState =
    MkTester $ do
        gts <- asks tcGetTableState
        lift gts
