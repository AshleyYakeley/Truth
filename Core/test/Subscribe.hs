module Subscribe where
{
    import Prelude;
    import Control.Monad.Trans.Class;
    import Truth.Core;
    import Test.Tasty;
    import Test.Tasty.HUnit;


    testEditor :: Editor (WholeEdit a) a;
    testEditor = let
    {
        editorInit (MkObject object) = do
        {
            initial <- object $ \muted -> lift $ mutableRead muted ReadWhole;
            return (initial,());
        };
        editorUpdate _ _ _ = return ();
        editorDo = return;
    } in MkEditor{..};

    testSavable :: TestTree;
    testSavable = testCase "Savable" $ do
    {
        object <- freeIOObject False (\_ -> True);
        MkSubscriptionW sub <- subscribeObject object;
        let
        {
            saveSub = MkSubscriptionW $ saveBufferSubscription sub;
            MkSubscriptionW cleanSaveSub = fmap fst saveSub;
        };
        found <- subscribeEdit cleanSaveSub testEditor;
        assertEqual "value" False found;
    };

    testSubscribe :: TestTree;
    testSubscribe = testGroup "subscribe"
    [
        testSavable
    ];
}
