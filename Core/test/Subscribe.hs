{-# OPTIONS -fno-warn-orphans #-}
module Subscribe(testSubscribe) where
{
    import Prelude;
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

    testEditor :: Editor (WholeEdit a) () a;
    testEditor = let
    {
        editorFirst = ();
        editorInit (MkObject object) = object $ \muted -> lift $ mutableRead muted ReadWhole;
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

    testOutputEditor :: forall edit actions. (Show edit, Show (EditSubject edit), FullReader (EditReader edit)) => String -> Handle -> MVar (EditSubject edit) -> (actions -> ([edit] -> IO ()) -> IO ()) -> Editor edit actions ();
    testOutputEditor name h var call = let
    {
        outputLn :: MonadIO m => String -> m ();
        outputLn s = liftIO $ hPutStrLn h $ name ++ ": " ++ s;

        editorFirst :: Int;
        editorFirst = 0;

        editorInit :: Object edit Int -> IO (Object edit Int);
        editorInit object = do
        {
            outputLn "Init";
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
            doEdits :: [edit] -> IO ();
            doEdits edits = do
            {
                runObject obj $ \muted -> do
                {
                    outputLn $ "push " ++ show edits;
                    maction <- lift $ mutableEdit muted edits;
                    case maction of
                    {
                        Nothing -> outputLn "push failed";
                        Just action -> do
                        {
                            n <- lift action;
                            outputLn $ "push succeeded -> " ++ show n;
                        };
                    }
                };
                withMVar var $ \s -> outputLn $ "var: " ++ show s;
            };
        } in do
        {
            withMVar var $ \s -> outputLn $ "var: " ++ show s;
            outputLn "Start";
            call actions doEdits;
            outputLn "End";
        };
    } in MkEditor{..};

    testString :: TestTree;
    testString = goldenTest "String" "test/String.ref" "test/String.out" $ \h -> do
    {
        var <- newMVar "ABCDE";
        let
        {
            varObj :: Object (WholeEdit String) ();
            varObj = mvarObject var $ \_ -> True;

            textObj :: Object (StringEdit String) ();
            textObj = convertObject varObj;

            editor :: Editor (StringEdit String) () ();
            editor = testOutputEditor "main" h var $ \_actions doEdits -> do
            {
                doEdits [StringReplaceSection (startEndRun 1 2) "xy"];
                doEdits [StringReplaceSection (startEndRun 2 4) "1"];
            };
        };
        MkSubscriptionW textSub <- makeObjectSubscriber textObj;
        subscribeEditor textSub editor;
    };

    testSubscribe :: TestTree;
    testSubscribe = testGroup "subscribe"
    [
        testSavable,
        testString
    ];
}
