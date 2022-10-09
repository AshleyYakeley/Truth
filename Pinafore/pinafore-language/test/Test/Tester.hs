module Test.Tester where

import Changes.Core
import Pinafore
import Pinafore.Test
import Shapes

data TesterContext = MkTesterContext
    { tcPinafore :: PinaforeContext
    , tcLibrary :: LibraryContext
    , tcGetTableState :: IO PinaforeTableSubject
    }

newtype Tester a =
    MkTester (ReaderT TesterContext View a)
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadException)

runTester :: FetchModule -> Tester () -> IO ()
runTester fm (MkTester ta) =
    withTestPinaforeContext fm stdout $ \getTableState ->
        runView $ runReaderT ta $ MkTesterContext ?pinafore ?library getTableState

testerRunAction :: PinaforeAction () -> Tester ()
testerRunAction pa =
    MkTester $
    ReaderT $ \MkTesterContext {..} -> let
        ?pinafore = tcPinafore
        in runPinaforeAction pa

testerLiftAction :: PinaforeAction --> Tester
testerLiftAction pa =
    MkTester $
    ReaderT $ \MkTesterContext {..} -> let
        ?pinafore = tcPinafore
        in do
               ka <- unliftPinaforeAction pa
               case ka of
                   Known a -> return a
                   Unknown -> fail "stopped"

testerLiftInterpreter :: PinaforeInterpreter --> Tester
testerLiftInterpreter pia =
    MkTester $
    ReaderT $ \MkTesterContext {..} -> let
        ?pinafore = tcPinafore
        ?library = tcLibrary
        in fromInterpretResult $ runPinaforeScoped "<input>" pia
