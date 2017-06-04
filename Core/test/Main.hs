{-# OPTIONS -fno-warn-orphans #-}
module Main(main) where
{
    import Prelude;
    import Data.Sequences;
    import Truth.Core;
    import Test.Tasty;
    import Test.Tasty.HUnit;
    import Test.Tasty.QuickCheck;


    instance (Arbitrary (Index seq),Integral (Index seq)) => Arbitrary (SequencePoint seq) where
    {
        arbitrary = MkSequencePoint <$> (getNonNegative <$> arbitrary);
    };

    instance (Arbitrary (Index seq),Integral (Index seq)) => Arbitrary (SequenceRun seq) where
    {
        arbitrary = MkSequenceRun <$> arbitrary <*> arbitrary;
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

    instance (Arbitrary seq,Arbitrary (Index seq),Integral (Index seq)) => Arbitrary (StringEdit seq) where
    {
        arbitrary = oneof [StringReplaceWhole <$> arbitrary,StringReplaceSection <$> arbitrary <*> arbitrary];
    };

    applyEditSubject :: (Edit edit,FullReader (EditReader edit)) => edit -> EditSubject edit -> EditSubject edit;
    applyEditSubject edit = fromReadFunction $ applyEdit edit;

    testEdit :: (Edit edit,FullReader (EditReader edit),Eq (EditSubject edit),Show edit,Show (EditSubject edit)) => edit -> EditSubject edit -> EditSubject edit -> TestTree;
    testEdit edit original expected = let
    {
        name = show edit ++ " " ++ show original;
        found = applyEditSubject edit original;
    } in testCase name $ assertEqual "" expected found;

    testEditRead :: (Edit edit,FullReader (EditReader edit),Eq t,Show t,Show edit,Show (EditSubject edit),Show (EditReader edit t)) => edit -> EditSubject edit -> EditReader edit t -> t -> TestTree;
    testEditRead edit original rt expected = let
    {
        name = show edit ++ " " ++ show original ++ " " ++ show rt;
        found = fromReadable (applyEdit edit rt) original;
    } in testCase name $ assertEqual "" expected found;

    seqRun :: Int -> Int -> SequenceRun [a];
    seqRun start len = MkSequenceRun (MkSequencePoint start) (MkSequencePoint len);

    testStringEdit :: TestTree;
    testStringEdit = testGroup "string edit" [
        testEdit (StringReplaceSection (seqRun 2 0) "XY") "ABCDE" "ABXYCDE",
        testEdit (StringReplaceSection (seqRun 2 1) "XY") "ABCDE" "ABXYDE",
        testEdit (StringReplaceSection (seqRun 2 2) "XY") "ABCDE" "ABXYE",
        testEdit (StringReplaceSection (seqRun 2 3) "XY") "ABCDE" "ABXY",
        testEdit (StringReplaceSection (seqRun 1 3) "XY") "ABCDE" "AXYE",
        testEditRead (StringReplaceSection (seqRun 2 0) "XY") "ABCDE" StringReadLength (MkSequencePoint 7),
        testEditRead (StringReplaceSection (seqRun 2 0) "XY") "ABCDE" (StringReadSection $ seqRun 0 7) "ABXYCDE",
        testEditRead (StringReplaceSection (seqRun 2 0) "XY") "ABCDE" (StringReadSection $ seqRun 0 3) "ABX",
        testEditRead (StringReplaceSection (seqRun 2 0) "XY") "ABCDE" (StringReadSection $ seqRun 1 3) "BXY",
        testEditRead (StringReplaceSection (seqRun 2 0) "XY") "ABCDE" (StringReadSection $ seqRun 1 4) "BXYC",
        testEditRead (StringReplaceSection (seqRun 2 0) "XY") "ABCDE" (StringReadSection $ seqRun 2 4) "XYCD",
        testEditRead (StringReplaceSection (seqRun 2 0) "XY") "ABCDE" (StringReadSection $ seqRun 3 3) "YCD"
        ];

    testLensGet :: TestTree;
    testLensGet = testProperty "get" $ \run (base :: String) -> let
    {
        MkFloatingEditLens{..} = stringSectionLens run;
        MkFloatingEditFunction{..} = floatingEditLensFunction;

        rf = floatingEditGet floatingEditInitial;

        found :: String;
        found = fromReadFunction rf base;

        expected :: String;
        expected = readFrom base $ StringReadSection run;
    } in found === expected;

    showVar :: Show a => String -> a -> String;
    showVar name val = name ++ " = " ++ show val;

    counterexamples :: [String] -> Property -> Property;
    counterexamples [] = id;
    counterexamples (s:ss) = counterexample s . counterexamples ss;

    lensUpdateGetProperty ::
    (
        Arbitrary edita,Show edita,Edit edita,
        FullReader (EditReader edita),
        Arbitrary (EditSubject edita),Show (EditSubject edita),
        Show editb,Edit editb,
        FullReader (EditReader editb),
        Eq (EditSubject editb),
        Show (EditSubject editb),
        Show state
    ) => FloatingEditLens' m state edita editb -> EditSubject edita -> edita -> Property;
    lensUpdateGetProperty lens oldA editA = let
    {
        newA = fromReadFunction (applyEdit editA) oldA;
        MkFloatingEditLens{..} = lens;
        MkFloatingEditFunction{..} = floatingEditLensFunction;
        oldB = fromReadFunction (floatingEditGet floatingEditInitial) oldA;
        rdb = floatingEditUpdate editA floatingEditInitial;
        (newState,editBs) = fromReadable rdb oldA;
        newB1 = fromReadFunction (applyEdits editBs) oldB;
        newB2 = fromReadFunction (floatingEditGet newState) newA;
        vars =
        [
            showVar "oldA" oldA,
            showVar "oldState" floatingEditInitial,
            showVar "oldB" oldB,
            showVar "editA" editA,
            showVar "editBs" editBs,
            showVar "newA" newA,
            showVar "newState" newState,
            showVar "newB (edits)" newB1,
            showVar "newB (lens )" newB2
        ];
    } in counterexamples vars $ newB1 === newB2;

    testLensUpdate :: TestTree;
    testLensUpdate = testProperty "update" $ \run (base :: String) edit -> lensUpdateGetProperty (stringSectionLens run) base edit;

    testStringSectionLens :: TestTree;
    testStringSectionLens = testGroup "string section lens" [
        testLensGet,
        localOption (QuickCheckTests 10000) testLensUpdate,
        localOption (QuickCheckTests 1) $ testGroup "update special" $ [
            testProperty "1 0" $ lensUpdateGetProperty (stringSectionLens $ seqRun 0 1) "A" (StringReplaceSection (seqRun 1 0) "x"),
            testProperty "4 1" $ lensUpdateGetProperty (stringSectionLens $ seqRun 0 5) "ABCDE" (StringReplaceSection (seqRun 4 1) "pqrstu"),
            testProperty "4 2" $ lensUpdateGetProperty (stringSectionLens $ seqRun 0 5) "ABCDE" (StringReplaceSection (seqRun 4 2) "pqrstu")
            ]
        ];

    tests :: TestTree;
    tests = testGroup "Truth-Core" [
        testStringEdit,
        testStringSectionLens
        ];

    main :: IO ();
    main = defaultMain tests;
}
