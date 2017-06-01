{-# OPTIONS -fno-warn-orphans #-}
module Main(main) where
{
    import Prelude;
    import Data.Sequences;
    import Truth.Core;
    import Test.Tasty;
    import Test.Tasty.HUnit;
    import Test.Tasty.QuickCheck;


    instance Arbitrary (Index seq) => Arbitrary (SequencePoint seq) where
    {
        arbitrary = MkSequencePoint <$> arbitrary;
    };

    instance Arbitrary (Index seq) => Arbitrary (SequenceRun seq) where
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
        show (StringReplaceSection run sq) = "StringReplaceSection " ++ show run ++ show sq;
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

    testStringSectionLens :: TestTree;
    testStringSectionLens = testGroup "string section lens" [
        testLensGet
        ];

    tests :: TestTree;
    tests = testGroup "Truth-Core" [
        testStringEdit,
        testStringSectionLens
        ];

    main :: IO ();
    main = defaultMain tests;
}
