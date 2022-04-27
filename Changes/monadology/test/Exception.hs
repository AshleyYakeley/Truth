module Exception
    ( testException
    ) where

import Control.Applicative
import Control.Monad.Ology
import Data.IORef
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

testACatch ::
       forall m e. (Eq e, Show e, MonadCatch e m)
    => m --> IO -> e -> TestTree
testACatch runM ex =
    testCase "catchExc" $ do
        r <- runM $ try @m @e $ throw @e @m @() ex
        assertEqual "caught" (FailureResult ex) r

testABracket ::
       forall m. (MonadTunnelIO m, MonadException m)
    => m --> IO -> (forall a. m a) -> TestTree
testABracket runM th =
    testCase "bracket" $ do
        ref <- newIORef False
        _ <- tryExc $ runM $ finally @m th (liftIO $ writeIORef ref True)
        b <- readIORef ref
        assertEqual "finally" True b

testAnException ::
       forall m e. (Eq e, Show e, MonadException m, MonadCatch e m, MonadTunnelIO m)
    => String
    -> m --> IO -> e -> TestTree
testAnException name runM ex = testGroup name [testACatch runM ex, testABracket runM $ throw ex]

runComposeInner :: ComposeInner Maybe IO --> IO
runComposeInner (MkComposeInner ima) = do
    ma <- ima
    case ma of
        Just a -> return a
        Nothing -> fail "Nothing"

runExceptT' :: ExceptT () IO --> IO
runExceptT' eia = do
    ea <- runExceptT eia
    case ea of
        Right a -> return a
        Left _ -> fail "Left"

testException :: TestTree
testException =
    testGroup
        "Exception"
        [ testAnException "IO" id $ ErrorCall "test"
        , testGroup "ExceptT () IO" $ pure $ testABracket @(ExceptT () IO) runExceptT' $ throwE ()
        , testGroup "ComposeInner Maybe IO" $
          pure $ testABracket @(ComposeInner Maybe IO) runComposeInner $ liftInner Nothing
        ]
