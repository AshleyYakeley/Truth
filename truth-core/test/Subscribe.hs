{-# OPTIONS -fno-warn-orphans #-}

module Subscribe
    ( testSubscribe
    ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.IsStateIO
import Data.Foldable
import Data.Sequences
import Prelude
import System.IO
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Truth.Core

goldenTest ::
       TestName
    -> FilePath
    -> FilePath
    -> ((?handle :: Handle) =>
            IO ())
    -> TestTree
goldenTest name refPath outPath call =
    goldenVsFile name refPath outPath $
    withBinaryFile outPath WriteMode $ \h -> let
        ?handle = h
        in call

goldenTest' ::
       TestName
    -> ((?handle :: Handle) =>
            IO ())
    -> TestTree
goldenTest' name call = goldenTest name ("test/golden/" ++ name ++ ".ref") ("test/golden/" ++ name ++ ".out") call

testEditor :: Editor (WholeEdit a) () a
testEditor = let
    editorInit (MkObject object) = object $ \muted -> mutableRead muted ReadWhole
    editorUpdate _ _ _ = return ()
    editorDo editor _ = return editor
    in MkEditor {..}

testSavable :: TestTree
testSavable =
    testCase "Savable" $ do
        object <- freeIOObject False (\_ -> True)
        sub <- makeObjectSubscriber object
        let
            saveSub = saveBufferSubscriber sub
            cleanSaveSub = fmap fst saveSub
        found <- subscribeEditor cleanSaveSub testEditor
        assertEqual "value" False found

instance Integral (Index seq) => Show (StringRead seq t) where
    show StringReadLength = "StringReadLength"
    show (StringReadSection run) = "StringReadSection " ++ show run

instance (Show (ReaderSubject reader)) => Show (WholeReaderEdit reader) where
    show (MkWholeEdit a) = "whole " ++ show a

instance (Show e1, Show e2) => Show (TupleEdit (PairSelector e1 e2)) where
    show (MkTupleEdit EditFirst e) = "fst " ++ show e
    show (MkTupleEdit EditSecond e) = "snd " ++ show e

instance (Show seq, Integral (Index seq)) => Show (StringEdit seq) where
    show (StringReplaceWhole sq) = "StringReplaceWhole " ++ show sq
    show (StringReplaceSection run sq) = "StringReplaceSection " ++ show run ++ " " ++ show sq

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
    editorInit :: Object edit -> IO (Object edit)
    editorInit object = do
        val <- runObject object $ \muted -> unReadable subjectFromReader $ mutableRead muted
        outputLn $ "init: " ++ show val
        return object
    editorUpdate ::
           forall m. IsStateIO m
        => Object edit
        -> MutableRead m (EditReader edit)
        -> [edit]
        -> m ()
    editorUpdate _ mr edits = do
        outputLn $ "receive " ++ show edits
        val <- unReadable subjectFromReader mr
        outputLn $ "receive " ++ show val
    editorDo :: Object edit -> actions -> IO ()
    editorDo obj subActions = let
        subDontEdits :: [[edit]] -> IO ()
        subDontEdits editss = do
            outputLn "runObject"
            runObject obj $ \muted ->
                for_ editss $ \edits -> do
                    outputLn $ "push " ++ show edits
                    maction <- mutableEdit muted edits
                    case maction of
                        Nothing -> outputLn "push disallowed"
                        Just _action -> outputLn "push ignored"
        subDoEdits :: [[edit]] -> IO ()
        subDoEdits editss = do
            outputLn "runObject"
            runObject obj $ \muted ->
                for_ editss $ \edits -> do
                    outputLn $ "push " ++ show edits
                    maction <- mutableEdit muted edits
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
    -> ((?handle :: Handle, ?showVar :: IO (), ?showExpected :: [edit] -> IO ()) =>
            Subscriber edit () -> IO ())
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
                    news <- fromReadFunctionM (applyEdits edits) $ return s
                    hPutStrLn ?handle $ "expected: " ++ show news
        call sub

testPair :: TestTree
testPair =
    testSubscription @(PairEdit (WholeEdit Bool) (WholeEdit Bool)) "Pair" (False, False) $ \sub ->
        subscribeEditor sub $
        testOutputEditor "main" $ \MkSubscribeContext {..} -> do
            ?showVar
            ?showExpected [MkTupleEdit EditFirst $ MkWholeEdit True, MkTupleEdit EditSecond $ MkWholeEdit True]
            subDoEdits [[MkTupleEdit EditFirst $ MkWholeEdit True, MkTupleEdit EditSecond $ MkWholeEdit True]]
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
        testLens <- fmap MkCloseState $ stringSectionLens (startEndRun 1 4)
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
