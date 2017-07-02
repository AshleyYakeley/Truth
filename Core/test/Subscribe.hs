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

    goldenTest :: TestName -> FilePath -> FilePath -> ((?handle :: Handle) => IO ()) -> TestTree;
    goldenTest name refPath outPath call = goldenVsFile name refPath outPath $ withBinaryFile outPath WriteMode $ \h -> let {?handle = h} in call;

    goldenTest' :: TestName -> ((?handle :: Handle) => IO ()) -> TestTree;
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
        MkSubscriberW sub <- makeObjectSubscriber object;
        let
        {
            saveSub = MkSubscriberW $ saveBufferSubscriber sub;
            MkSubscriberW cleanSaveSub = fmap fst saveSub;
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

    data SubscribeContext edit actions = MkSubscribeContext
    {
        subDoEdits :: [[edit]] -> IO (),
        subDontEdits :: [[edit]] -> IO (),
        subActions :: actions
    };

    testOutputEditor :: forall edit actions. (Show edit, Show (EditSubject edit), FullReader (EditReader edit),?handle::Handle) => String -> (SubscribeContext edit actions -> IO ()) -> Editor edit actions ();
    testOutputEditor name call = let
    {
        outputLn :: MonadIO m => String -> m ();
        outputLn s = liftIO $ hPutStrLn ?handle $ name ++ ": " ++ s;

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
        editorDo obj subActions = let
        {
            subDontEdits :: [[edit]] -> IO ();
            subDontEdits editss = do
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

            subDoEdits :: [[edit]] -> IO ();
            subDoEdits editss = do
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
        } in call MkSubscribeContext{..};
    } in MkEditor{..};

    testSubscription :: forall edit. (FullEdit edit,Show (EditSubject edit)) =>
        TestName -> EditSubject edit -> ((?handle :: Handle,?showVar :: IO (),?showExpected :: [edit] -> IO ()) => Subscriber edit () -> IO ()) -> TestTree;
    testSubscription name initial call = goldenTest' name $ do
    {
        var <- newMVar initial;
        let
        {
            varObj :: Object (WholeEdit (EditSubject edit)) ();
            varObj = mvarObject var $ \_ -> True;

            editObj :: Object edit ();
            editObj = convertObject varObj;
        };
        MkSubscriberW sub <- makeObjectSubscriber editObj;
        let
        {
            ?showVar = withMVar var $ \s -> hPutStrLn ?handle $ "var: " ++ show s;
            ?showExpected = \edits -> withMVar var $ \s -> hPutStrLn ?handle $ "expected: " ++ show (fromReadFunction (applyEdits edits) s);
        }
        in call sub;
    };

    testString :: TestTree;
    testString = testSubscription "String" "ABCDE" $ \sub ->
        subscribeEditor sub $ testOutputEditor "main" $ \MkSubscribeContext{..} -> do
        {
            ?showVar;
            subDontEdits [[StringReplaceSection (startEndRun 3 5) "PQR"]];
            ?showVar;
            subDontEdits [[StringReplaceSection (startEndRun 2 3) ""]];
            ?showVar;
            subDoEdits [[StringReplaceSection (startEndRun 1 2) "xy"]];
            ?showVar;
            subDoEdits [[StringReplaceSection (startEndRun 2 4) "1"]];
            ?showVar;
        };

    testString1 :: TestTree;
    testString1 = testSubscription "String1" "ABCDE" $ \sub ->
        subscribeEditor sub $ testOutputEditor "main" $ \MkSubscribeContext{..} -> do
        {
            ?showVar;
            subDontEdits [[StringReplaceSection (startEndRun 3 5) "PQR"],[StringReplaceSection (startEndRun 2 3) ""]];
            ?showVar;
            subDoEdits [[StringReplaceSection (startEndRun 1 2) "xy"],[StringReplaceSection (startEndRun 2 4) "1"]];
            ?showVar;
        };

    testString2 :: TestTree;
    testString2 = testSubscription "String2" "ABCDE" $ \sub ->
        subscribeEditor sub $ testOutputEditor "main" $ \MkSubscribeContext{..} -> do
        {
            ?showVar;
            subDontEdits [[StringReplaceSection (startEndRun 3 5) "PQR",StringReplaceSection (startEndRun 2 3) ""]];
            ?showVar;
            ?showExpected [StringReplaceSection (startEndRun 1 2) "xy",StringReplaceSection (startEndRun 2 4) "1"];
            subDoEdits [[StringReplaceSection (startEndRun 1 2) "xy",StringReplaceSection (startEndRun 2 4) "1"]];
            ?showVar;
        };

    testSharedString :: TestTree;
    testSharedString = testSubscription "SharedString" "ABCDE" $ \sub -> let
        {
            testLens :: GeneralLens' Maybe (StringEdit String) (StringEdit String);
            testLens = toGeneralLens' $ stringSectionLens (startEndRun 1 4);
        } in
        subscribeEditor sub $ testOutputEditor "main" $ \MkSubscribeContext{..} ->
        subscribeEditor (mapSubscriber testLens sub) $ testOutputEditor "lens" $ \_ -> do
        {
            ?showVar;
            subDontEdits [[StringReplaceSection (startEndRun 3 5) "PQR"]];
            ?showVar;
            subDoEdits [[StringReplaceSection (startEndRun 1 2) "xy"]];
            ?showVar;
            subDoEdits [[StringReplaceSection (startEndRun 2 4) "1"]];
            ?showVar;
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
