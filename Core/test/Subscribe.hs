{-# OPTIONS -fno-warn-orphans #-}
module Subscribe(testSubscribe) where
{
    import Prelude;
    import Data.Foldable;
    import System.IO;
    import Control.Concurrent.MVar;
    import Control.Monad.IO.Class;
    import Control.Monad.Trans.Class;
    import Control.Monad.Trans.State.Extra;
    import Control.Monad.IsStateIO;
    import Data.Sequences;
    import Truth.Core;
    import Test.Tasty;
    import Test.Tasty.HUnit;
    import Test.Tasty.Golden;

    goldenTest :: TestName -> FilePath -> FilePath -> (Handle -> IO ()) -> TestTree;
    goldenTest name refPath outPath call = goldenVsFile name refPath outPath $ withBinaryFile outPath WriteMode call;

    goldenTest' :: TestName -> (Handle -> IO ()) -> TestTree;
    goldenTest' name call = goldenTest name ("test/golden/" ++ name ++ ".ref") ("test/golden/" ++ name ++ ".out") call;

    testEditor :: Editor (WholeEdit a) () a;
    testEditor = let
    {
        editorFirst = ();
        editorInit (MkObject object) = object $ \muted _acc -> mutableRead muted ReadWhole;
        editorUpdate _ _ _ = return ();
        editorDo editor _ = return editor;
    } in MkEditor{..};

    testSavable :: TestTree;
    testSavable = testCase "Savable" $ do
    {
        object <- freeIOObject False (\_ -> True);
        MkSubscriptionW sub <- makeObjectSubscriber object;
        let
        {
            saveSub = MkSubscriptionW $ saveBufferSubscription sub;
            MkSubscriptionW cleanSaveSub = fmap fst saveSub;
        };
        found <- subscribeEditor cleanSaveSub testEditor;
        assertEqual "value" False found;
    };

    instance Integral (Index seq) => Show (StringRead seq t) where
    {
        show StringReadLength = "StringReadLength";
        show (StringReadSection run) = "StringReadSection " ++ show run;
    };

    instance (Show seq,Integral (Index seq)) => Show (StringEdit seq) where
    {
        show (StringReplaceWhole sq) = "StringReplaceWhole " ++ show sq;
        show (StringReplaceSection run sq) = "StringReplaceSection " ++ show run ++ " " ++ show sq;
    };

    testOutputEditor :: forall edit actions. (Show edit, Show (EditSubject edit), FullReader (EditReader edit)) => String -> Handle -> (actions -> ([[edit]] -> IO ()) -> ([[edit]] -> IO ()) -> IO ()) -> Editor edit actions ();
    testOutputEditor name h call = let
    {
        outputLn :: MonadIO m => String -> m ();
        outputLn s = liftIO $ hPutStrLn h $ name ++ ": " ++ s;

        editorFirst :: Int;
        editorFirst = 0;

        editorInit :: Object edit Int -> IO (Object edit Int);
        editorInit object = do
        {
            val <- runObject object $ \muted _acc -> unReadable fromReader $ mutableRead muted;
            outputLn $ "init: " ++ show val;
            return object;
        };

        editorUpdate :: forall m. IsStateIO m => Object edit Int -> MutableRead m (EditReader edit) -> [edit] -> StateT Int m ();
        editorUpdate _ mr edits = do
        {
            i <- get;
            outputLn $ "receive " ++ show i ++ ": " ++ show edits;
            str <- lift $ unReadable fromReader mr;
            outputLn $ "receive " ++ show i ++ ": " ++ show str;
            put $ i + 1;
        };

        editorDo :: Object edit Int -> actions -> IO ();
        editorDo obj actions = let
        {
            dontEdits :: [[edit]] -> IO ();
            dontEdits editss = do
            {
                outputLn "runObject";
                runObject obj $ \muted _acc -> for_ editss $ \edits -> do
                {
                    outputLn $ "push " ++ show edits;
                    maction <- mutableEdit muted edits;
                    case maction of
                    {
                        Nothing -> outputLn "push disallowed";
                        Just _action -> outputLn "push ignored";
                    }
                };
            };

            doEdits :: [[edit]] -> IO ();
            doEdits editss = do
            {
                outputLn "runObject";
                runObject obj $ \muted acc -> for_ editss $ \edits -> do
                {
                    outputLn $ "push " ++ show edits;
                    maction <- mutableEdit muted edits;
                    case maction of
                    {
                        Nothing -> outputLn "push disallowed";
                        Just action -> do
                        {
                            action;
                            n <- acc get;
                            outputLn $ "push succeeded -> " ++ show n;
                        };
                    }
                };
            };
        } in call actions doEdits dontEdits;
    } in MkEditor{..};

    testString :: TestTree;
    testString = goldenTest' "String" $ \h -> do
    {
        var <- newMVar "ABCDE";
        let
        {
            showVar :: IO ();
            showVar = withMVar var $ \s -> hPutStrLn h $ "var: " ++ show s;

            varObj :: Object (WholeEdit String) ();
            varObj = mvarObject var $ \_ -> True;

            textObj :: Object (StringEdit String) ();
            textObj = convertObject varObj;
        };
        MkSubscriptionW textSub <- makeObjectSubscriber textObj;
        subscribeEditor textSub $ testOutputEditor "main" h $ \_actions doEdits dontEdits -> do
        {
            showVar;
            dontEdits [[StringReplaceSection (startEndRun 3 5) "PQR"]];
            showVar;
            dontEdits [[StringReplaceSection (startEndRun 2 3) ""]];
            showVar;
            doEdits [[StringReplaceSection (startEndRun 1 2) "xy"]];
            showVar;
            doEdits [[StringReplaceSection (startEndRun 2 4) "1"]];
            showVar;
        };
    };

    testString1 :: TestTree;
    testString1 = goldenTest' "String1" $ \h -> do
    {
        var <- newMVar "ABCDE";
        let
        {
            showVar :: IO ();
            showVar = withMVar var $ \s -> hPutStrLn h $ "var: " ++ show s;

            varObj :: Object (WholeEdit String) ();
            varObj = mvarObject var $ \_ -> True;

            textObj :: Object (StringEdit String) ();
            textObj = convertObject varObj;
        };
        MkSubscriptionW textSub <- makeObjectSubscriber textObj;
        subscribeEditor textSub $ testOutputEditor "main" h $ \_actions doEdits dontEdits -> do
        {
            showVar;
            dontEdits [[StringReplaceSection (startEndRun 3 5) "PQR"],[StringReplaceSection (startEndRun 2 3) ""]];
            showVar;
            doEdits [[StringReplaceSection (startEndRun 1 2) "xy"],[StringReplaceSection (startEndRun 2 4) "1"]];
            showVar;
        };
    };

    testString2 :: TestTree;
    testString2 = goldenTest' "String2" $ \h -> do
    {
        var <- newMVar "ABCDE";
        let
        {
            showVar :: IO ();
            showVar = withMVar var $ \s -> hPutStrLn h $ "var: " ++ show s;

            showExpected :: [StringEdit String] -> IO ();
            showExpected edits = withMVar var $ \s -> hPutStrLn h $ "expected: " ++ show (fromReadFunction (applyEdits edits) s);

            varObj :: Object (WholeEdit String) ();
            varObj = mvarObject var $ \_ -> True;

            textObj :: Object (StringEdit String) ();
            textObj = convertObject varObj;
        };
        MkSubscriptionW textSub <- makeObjectSubscriber textObj;
        subscribeEditor textSub $ testOutputEditor "main" h $ \_actions doEdits dontEdits -> do
        {
            showVar;
            dontEdits [[StringReplaceSection (startEndRun 3 5) "PQR",StringReplaceSection (startEndRun 2 3) ""]];
            showVar;
            showExpected [StringReplaceSection (startEndRun 1 2) "xy",StringReplaceSection (startEndRun 2 4) "1"];
            doEdits [[StringReplaceSection (startEndRun 1 2) "xy",StringReplaceSection (startEndRun 2 4) "1"]];
            showVar;
        };
    };

    testSharedString :: TestTree;
    testSharedString = goldenTest' "SharedString" $ \h -> do
    {
        var <- newMVar "ABCDE";
        let
        {
            showVar :: IO ();
            showVar = withMVar var $ \s -> hPutStrLn h $ "var: " ++ show s;

            varObj :: Object (WholeEdit String) ();
            varObj = mvarObject var $ \_ -> True;

            textObj :: Object (StringEdit String) ();
            textObj = convertObject varObj;

            testLens :: GeneralLens' Maybe (StringEdit String) (StringEdit String);
            testLens = toGeneralLens' $ stringSectionLens (startEndRun 1 4);
        };
        MkSubscriptionW textSub <- makeObjectSubscriber textObj;
        subscribeEditor textSub $ testOutputEditor "main" h $ \_ mainDoEdits mainDontEdits ->
        subscribeEditor (mapSubscription testLens textSub) $ testOutputEditor "lens" h $ \_ _lensDoEdits _lensDontEdits -> do
        {
            showVar;
            mainDontEdits [[StringReplaceSection (startEndRun 3 5) "PQR"]];
            showVar;
            mainDoEdits [[StringReplaceSection (startEndRun 1 2) "xy"]];
            showVar;
            mainDoEdits [[StringReplaceSection (startEndRun 2 4) "1"]];
            showVar;
        };
    };

    testSubscribe :: TestTree;
    testSubscribe = testGroup "subscribe"
    [
        testSavable,
        testString,
        testString1,
        testString2,
        testSharedString
    ];
}
