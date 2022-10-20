module Test.Tester where

import Changes.Core
import Pinafore
import Pinafore.Test
import Shapes

data TesterContext = MkTesterContext
    { tcPinafore :: QContext
    , tcLibrary :: LibraryContext
    , tcGetTableState :: IO QTableSubject
    }

newtype Tester a =
    MkTester (ReaderT TesterContext View a)
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadException)

runTester :: FetchModule -> Tester () -> IO ()
runTester fm (MkTester ta) =
    withTestQContext fm stdout $ \getTableState ->
        runView $ runReaderT ta $ MkTesterContext ?qcontext ?library getTableState

testerRunAction :: Action () -> Tester ()
testerRunAction pa =
    MkTester $
    ReaderT $ \MkTesterContext {..} -> let
        ?qcontext = tcPinafore
        in runAction pa

testerLiftAction :: Action --> Tester
testerLiftAction pa =
    MkTester $
    ReaderT $ \MkTesterContext {..} -> let
        ?qcontext = tcPinafore
        in do
               ka <- unliftAction pa
               case ka of
                   Known a -> return a
                   Unknown -> fail "stopped"

testerLiftInterpreter :: QInterpreter --> Tester
testerLiftInterpreter pia =
    MkTester $
    ReaderT $ \MkTesterContext {..} -> let
        ?qcontext = tcPinafore
        ?library = tcLibrary
        in fromInterpretResult $ runPinaforeScoped "<input>" pia
