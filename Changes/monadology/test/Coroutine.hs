module Coroutine
    ( testCoroutine
    ) where

import Control.Monad.Ology
import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Useful

testCoroutineM ::
       forall m. (MonadIO m, MonadCoroutine m)
    => String
    -> (m () -> IO ())
    -> TestTree
testCoroutineM name run =
    testCase name $
    compareTest "+A+X-A+B-X" $ \appendStr -> do
        let
            output :: String -> m ()
            output s = liftIO $ appendStr s
            f1 :: (() -> m ()) -> m ()
            f1 y = do
                withMessage output "A" $ y ()
                withMessage output "B" $ y ()
            f2 :: (() -> m ()) -> m ()
            f2 y = withMessage output "X" $ y ()
        run $ runCoroutine $ joinCoroutines (coroutineSuspend f1) (\() -> coroutineSuspend f2)

testCoroutine :: TestTree
testCoroutine =
    testGroup
        "coroutine"
        [testCoroutineM @IO "IO" id] {-, testCoroutineM @(ContT () IO) "ContT () IO" $ \c -> runContT c return -}
