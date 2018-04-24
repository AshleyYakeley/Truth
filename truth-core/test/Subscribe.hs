{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoOverloadedStrings #-}

module Subscribe
    ( testSubscribe
    ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Unlift
import Data.Foldable
import Prelude
import System.IO
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Truth.Core

goldenTest :: TestName -> FilePath -> FilePath -> ((?handle :: Handle) => IO ()) -> TestTree
goldenTest name refPath outPath call =
    goldenVsFile name refPath outPath $
    withBinaryFile outPath WriteMode $ \h -> let
        ?handle = h
        in call

goldenTest' :: TestName -> ((?handle :: Handle) => IO ()) -> TestTree
goldenTest' name call = goldenTest name ("test/golden/" ++ name ++ ".ref") ("test/golden/" ++ name ++ ".out") call

{-
testEditor :: Editor (WholeEdit a) () a
testEditor = let
    editorInit (MkObject run r _) = run $ r ReadWhole
    editorUpdate _ _ _ = return ()
    editorDo editor _ = return editor
    in MkEditor {..}
-}
testSavable :: TestTree
testSavable = testCase "Savable" $ do return ()

{-
        object <- freeIOObject False (\_ -> True)
        sub <- makeObjectSubscriber object
        let
            saveSub = saveBufferSubscriber sub
            cleanSaveSub = fmap fst saveSub
        found <- subscribeEditor cleanSaveSub testEditor
        assertEqual "value" False found
-}
data SubscribeContext edit actions = MkSubscribeContext
    { subDoEdits :: [[edit]] -> IO ()
    , subDontEdits :: [[edit]] -> IO ()
    , subActions :: actions
    }

testOutputEditor ::
       forall edit actions. (Show edit, Show (EditSubject edit), FullSubjectReader (EditReader edit), ?handle :: Handle)
    => String
    -> (SubscribeContext edit actions -> IO ())
    -> Editor edit actions ()
testOutputEditor name call = let
    outputLn :: MonadIO m => String -> m ()
    outputLn s = liftIO $ hPutStrLn ?handle $ name ++ ": " ++ s
    editorInit :: Object edit -> IO ()
    editorInit (MkObject (MkUnliftIO run) r _) = do
        val <- run $ mutableReadToSubject r
        outputLn $ "init: " ++ show val
        return ()
    editorUpdate :: () -> Object edit -> [edit] -> IO ()
    editorUpdate () (MkObject (MkUnliftIO run) mr _) edits = do
        outputLn $ "receive " ++ show edits
        val <- run $ mutableReadToSubject mr
        outputLn $ "receive " ++ show val
    editorDo :: () -> Object edit -> actions -> IO ()
    editorDo () (MkObject (MkUnliftIO run) _ push) subActions = let
        subDontEdits :: [[edit]] -> IO ()
        subDontEdits editss = do
            outputLn "runObject"
            run $
                for_ editss $ \edits -> do
                    outputLn $ "push " ++ show edits
                    maction <- push edits
                    case maction of
                        Nothing -> outputLn "push disallowed"
                        Just _action -> outputLn "push ignored"
        subDoEdits :: [[edit]] -> IO ()
        subDoEdits editss = do
            outputLn "runObject"
            run $
                for_ editss $ \edits -> do
                    outputLn $ "push " ++ show edits
                    maction <- push edits
                    case maction of
                        Nothing -> outputLn "push disallowed"
                        Just action -> do
                            action
                            outputLn $ "push succeeded"
        in call MkSubscribeContext {..}
    in MkEditor {..}

testSubscription ::
       forall edit. (FullEdit edit, Show (EditSubject edit))
    => TestName
    -> EditSubject edit
    -> ((?handle :: Handle, ?showVar :: IO (), ?showExpected :: [edit] -> IO ()) => Subscriber edit () -> IO ())
    -> TestTree
testSubscription name initial call =
    goldenTest' name $ do
        var <- newMVar initial
        let
            varObj :: Object (WholeEdit (EditSubject edit))
            varObj = mvarObject var $ \_ -> True
            editObj :: Object edit
            editObj = convertObject varObj
        sub <- makeObjectSubscriber editObj
        let
            ?showVar = withMVar var $ \s -> hPutStrLn ?handle $ "var: " ++ show s
            ?showExpected = \edits ->
                withMVar var $ \s -> do
                    news <- mutableReadToSubject $ applyEdits edits $ subjectToMutableRead s
                    hPutStrLn ?handle $ "expected: " ++ show news
        call sub

testPair :: TestTree
testPair =
    testSubscription @(PairEdit (WholeEdit Bool) (WholeEdit Bool)) "Pair" (False, False) $ \sub ->
        subscribeEditor sub $
        testOutputEditor "main" $ \MkSubscribeContext {..} -> do
            ?showVar
            ?showExpected [MkTupleEdit SelectFirst $ MkWholeEdit True, MkTupleEdit SelectSecond $ MkWholeEdit True]
            subDoEdits [[MkTupleEdit SelectFirst $ MkWholeEdit True, MkTupleEdit SelectSecond $ MkWholeEdit True]]
            ?showVar

testString :: TestTree
testString =
    testSubscription "String" "ABCDE" $ \sub ->
        subscribeEditor sub $
        testOutputEditor "main" $ \MkSubscribeContext {..} -> do
            ?showVar
            subDontEdits [[StringReplaceSection (startEndRun 3 5) "PQR"]]
            ?showVar
            subDontEdits [[StringReplaceSection (startEndRun 2 3) ""]]
            ?showVar
            subDoEdits [[StringReplaceSection (startEndRun 1 2) "xy"]]
            ?showVar
            subDoEdits [[StringReplaceSection (startEndRun 2 4) "1"]]
            ?showVar

testString1 :: TestTree
testString1 =
    testSubscription "String1" "ABCDE" $ \sub ->
        subscribeEditor sub $
        testOutputEditor "main" $ \MkSubscribeContext {..} -> do
            ?showVar
            subDontEdits [[StringReplaceSection (startEndRun 3 5) "PQR"], [StringReplaceSection (startEndRun 2 3) ""]]
            ?showVar
            subDoEdits [[StringReplaceSection (startEndRun 1 2) "xy"], [StringReplaceSection (startEndRun 2 4) "1"]]
            ?showVar

testString2 :: TestTree
testString2 =
    testSubscription "String2" "ABCDE" $ \sub ->
        subscribeEditor sub $
        testOutputEditor "main" $ \MkSubscribeContext {..} -> do
            ?showVar
            subDontEdits [[StringReplaceSection (startEndRun 3 5) "PQR", StringReplaceSection (startEndRun 2 3) ""]]
            ?showVar
            ?showExpected [StringReplaceSection (startEndRun 1 2) "xy", StringReplaceSection (startEndRun 2 4) "1"]
            subDoEdits [[StringReplaceSection (startEndRun 1 2) "xy", StringReplaceSection (startEndRun 2 4) "1"]]
            ?showVar

testSharedString :: TestTree
testSharedString =
    testSubscription "SharedString" "ABCDE" $ \sub -> do
        testLens <- stringSectionLens (startEndRun 1 4)
        subscribeEditor sub $
            testOutputEditor "main" $ \MkSubscribeContext {..} ->
                subscribeEditor (mapSubscriber testLens sub) $
                testOutputEditor "lens" $ \_ -> do
                    ?showVar
                    subDontEdits [[StringReplaceSection (startEndRun 3 5) "PQR"]]
                    ?showVar
                    subDoEdits [[StringReplaceSection (startEndRun 1 2) "xy"]]
                    ?showVar
                    subDoEdits [[StringReplaceSection (startEndRun 2 4) "1"]]
                    ?showVar

testSubscribe :: TestTree
testSubscribe = testGroup "subscribe" [testSavable, testPair, testString, testString1, testString2, testSharedString]
