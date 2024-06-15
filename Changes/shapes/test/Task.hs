module Task
    ( testTask
    ) where

import Shapes
import Shapes.Test

testRecord ::
       forall a r. (Eq a, Show a, Monoid a)
    => a
    -> ((a -> IO ()) -> IO r)
    -> IO r
testRecord expected call = do
    var <- newMVar mempty
    r <- call $ \a -> modifyMVar_ var $ \s -> return $ s <> a
    found <- takeMVar var
    assertEqual "record" expected found
    return r

testTask1 :: TestTree
testTask1 =
    testTree "1" $
    testRecord @String "[1]" $ \record -> do
        record "["
        stask <-
            forkStoppableTask $ \_ -> do
                record "1"
                threadSleep 0.2
                record "2"
        let task = stoppableTaskTask stask
        threadSleep 0.1
        stoppableTaskStop stask
        threadSleep 0.3
        done <- taskIsDone task
        assertEqual "done" True done
        result <- taskWait task
        assertEqual "result" Nothing result
        record "]"

testTask2 :: TestTree
testTask2 =
    testTree "2" $
    testRecord @String "[1]" $ \record -> do
        record "["
        stask <-
            forkStoppableTask $ \stop -> do
                record "1"
                threadSleep 0.1
                () <- stop
                record "2"
        let task = stoppableTaskTask stask
        threadSleep 0.2
        done <- taskIsDone task
        assertEqual "done" True done
        result <- taskWait task
        assertEqual "result" Nothing result
        record "]"

testTask :: TestTree
testTask = testTree "task" [testTask1, testTask2]
