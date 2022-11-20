module Pinafore.Test
    ( parseTokens
    , parseType
    , runInterpreter
    , QTypeSystem
    , Name
    , VarID
    , mkVarID
    , firstVarIDState
    , UVar
    , Var(..)
    , QGroundType(..)
    , EntityGroundType(..)
    , QValue
    , QType
    , QPolyShim
    , QOpenExpression
    , QExpression
    , QShimWit
    , QSingularType
    , QSingularShimWit
    , QInterpreter
    , toJMShimWit
    , allocateVar
    , QScopeInterpreter
    , registerType
    , registerLetBindings
    , registerLetBinding
    , registerPatternConstructor
    , registerSubtypeConversion
    , module Pinafore.Language.Expression
    , checkUpdateEditor
    , QTableSubject(..)
    , makeTestInvocationInfo
    , TesterOptions(..)
    , defaultTester
    , Tester
    , runTester
    , testerLiftView
    , testerLiftAction
    , testerRunAction
    , testerLiftInterpreter
    , testerGetTableState
    ) where

import Changes.Core
import Pinafore
import Pinafore.Language.Expression
import Pinafore.Language.Grammar
import Pinafore.Language.Grammar.Read.Token
import Pinafore.Language.Interpreter
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Language.VarID
import Shapes

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
    { tstFetchModule :: FetchModule ()
    , tstOutput :: Handle
    }

defaultTester :: TesterOptions
defaultTester = let
    tstFetchModule = mempty
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
        let library = mkLibraryContext ii $ contramap (\_ -> ()) tstFetchModule
        runView $ runReaderT ta $ MkTesterContext ii library getTableState

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

testerGetTableState :: Tester QTableSubject
testerGetTableState =
    MkTester $ do
        gts <- asks tcGetTableState
        lift gts
