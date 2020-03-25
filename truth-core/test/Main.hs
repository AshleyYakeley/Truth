{-# OPTIONS -fno-warn-orphans #-}

module Main
    ( main
    ) where

import Lens
import Resource
import Shapes
import Subscribe
import Test.SimpleString
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Truth.Core

instance (Arbitrary (Index seq), Integral (Index seq)) => Arbitrary (SequencePoint seq) where
    arbitrary = MkSequencePoint <$> (getSmall . getNonNegative <$> arbitrary)
    shrink 0 = []
    shrink 1 = [0]
    shrink n = [0, pred n]

instance (Arbitrary (Index seq), Integral (Index seq)) => Arbitrary (SequenceRun seq) where
    arbitrary = MkSequenceRun <$> arbitrary <*> arbitrary
    shrink (MkSequenceRun s l) = [MkSequenceRun s' l' | (s', l') <- shrink (s, l)]

instance (Arbitrary seq, Arbitrary (Index seq), Integral (Index seq)) => Arbitrary (StringEdit seq) where
    arbitrary = oneof [StringReplaceWhole <$> arbitrary, StringReplaceSection <$> arbitrary <*> arbitrary]
    shrink (StringReplaceWhole s) = StringReplaceWhole <$> shrink s
    shrink (StringReplaceSection r s) =
        (StringReplaceWhole s) : [StringReplaceSection r' s' | (r', s') <- shrink (r, s)]

instance {-# OVERLAPPING #-} Arbitrary (StringEdit String) where
    arbitrary =
        oneof
            [ StringReplaceWhole <$> (getSimpleString <$> arbitrary)
            , StringReplaceSection <$> arbitrary <*> (getSimpleString <$> arbitrary)
            ]
    shrink (StringReplaceWhole s) = StringReplaceWhole <$> (getSimpleString <$> shrink (MkSimpleString s))
    shrink (StringReplaceSection r s) =
        (StringReplaceWhole s) : [StringReplaceSection r' s' | (r', MkSimpleString s') <- shrink (r, MkSimpleString s)]

testApplyEditsPar :: TestTree
testApplyEditsPar =
    testCase "apply edits parallel" $ let
        start = (False, False)
        edits :: [PairUpdateEdit (WholeUpdate Bool) (WholeUpdate Bool)]
        edits =
            [ MkTupleUpdateEdit SelectFirst $ MkWholeReaderEdit True
            , MkTupleUpdateEdit SelectSecond $ MkWholeReaderEdit True
            ]
        expected = (True, True)
        rf :: ReadFunction (TupleUpdateReader (PairSelector (WholeUpdate Bool) (WholeUpdate Bool))) (TupleUpdateReader (PairSelector (WholeUpdate Bool) (WholeUpdate Bool)))
        rf = applyEdits edits
        in do
               found <- readableToSubject $ rf $ subjectToReadable start
               assertEqual "" expected found

testApplyEditsSeq :: TestTree
testApplyEditsSeq =
    testCase "apply edits sequence" $ let
        start = 0
        edits :: [WholeEdit Int]
        edits = [MkWholeReaderEdit 1, MkWholeReaderEdit 2]
        expected = 2
        rf :: ReadFunction (WholeReader Int) (WholeReader Int)
        rf = applyEdits edits
        in do
               found <- readableToSubject $ rf $ subjectToReadable start
               assertEqual "" expected found

applyEditSubject ::
       (ApplicableEdit edit, FullSubjectReader (EditReader edit)) => edit -> EditSubject edit -> IO (EditSubject edit)
applyEditSubject edit subj = readableToSubject $ applyEdit edit $ subjectToReadable subj

testEdit ::
       ( ApplicableEdit edit
       , FullSubjectReader (EditReader edit)
       , Eq (EditSubject edit)
       , Show edit
       , Show (EditSubject edit)
       )
    => edit
    -> EditSubject edit
    -> EditSubject edit
    -> TestTree
testEdit edit original expected = let
    name = show edit ++ " " ++ show original
    in testCase name $ do
           found <- applyEditSubject edit original
           assertEqual "" expected found

testEditRead ::
       ( SubjectReader (EditReader edit)
       , ApplicableEdit edit
       , Eq t
       , Show t
       , Show edit
       , Show (EditSubject edit)
       , Show (EditReader edit t)
       )
    => edit
    -> EditSubject edit
    -> EditReader edit t
    -> t
    -> TestTree
testEditRead edit original rt expected = let
    name = show edit ++ " " ++ show original ++ " " ++ show rt
    in testCase name $ do
           found <- applyEdit edit (subjectToReadable original) rt
           assertEqual "" expected found

seqRun :: Int -> Int -> SequenceRun [a]
seqRun start len = MkSequenceRun (MkSequencePoint start) (MkSequencePoint len)

testStringEdit :: TestTree
testStringEdit =
    testGroup
        "string edit"
        [ testEdit (StringReplaceSection (seqRun 2 0) "XY") "ABCDE" "ABXYCDE"
        , testEdit (StringReplaceSection (seqRun 2 1) "XY") "ABCDE" "ABXYDE"
        , testEdit (StringReplaceSection (seqRun 2 2) "XY") "ABCDE" "ABXYE"
        , testEdit (StringReplaceSection (seqRun 2 3) "XY") "ABCDE" "ABXY"
        , testEdit (StringReplaceSection (seqRun 1 3) "XY") "ABCDE" "AXYE"
        , testEditRead (StringReplaceSection (seqRun 2 0) "XY") "ABCDE" StringReadLength (MkSequencePoint 7)
        , testEditRead (StringReplaceSection (seqRun 2 0) "XY") "ABCDE" (StringReadSection $ seqRun 0 7) "ABXYCDE"
        , testEditRead (StringReplaceSection (seqRun 2 0) "XY") "ABCDE" (StringReadSection $ seqRun 0 3) "ABX"
        , testEditRead (StringReplaceSection (seqRun 2 0) "XY") "ABCDE" (StringReadSection $ seqRun 1 3) "BXY"
        , testEditRead (StringReplaceSection (seqRun 2 0) "XY") "ABCDE" (StringReadSection $ seqRun 1 4) "BXYC"
        , testEditRead (StringReplaceSection (seqRun 2 0) "XY") "ABCDE" (StringReadSection $ seqRun 2 4) "XYCD"
        , testEditRead (StringReplaceSection (seqRun 2 0) "XY") "ABCDE" (StringReadSection $ seqRun 3 3) "YCD"
        ]

testLensGet :: TestTree
testLensGet =
    testProperty "get" $ \srun (base :: String) ->
        ioProperty $
        runLifeCycle $ do
            MkFloatingEditLens {..} <- return $ stringSectionLens srun
            r <- runFloatInit felInit $ subjectToReadable base
            let
                expected :: String
                expected = subjectToRead base $ StringReadSection srun
            found <- readableToSubject $ elGet (felLens r) $ subjectToReadable @LifeCycleIO base
            return $ found === expected

showVar :: Show a => String -> a -> String
showVar name val = name ++ " = " ++ show val

counterexamples :: [String] -> Property -> Property
counterexamples [] = id
counterexamples (s:ss) = counterexample s . counterexamples ss

lensUpdateGetProperty ::
       forall state updateA updateB.
       ( IsUpdate updateA
       , Show (UpdateEdit updateA)
       , ApplicableEdit (UpdateEdit updateA)
       , FullSubjectReader (UpdateReader updateA)
       , Show (UpdateSubject updateA)
       , IsEditUpdate updateB
       , Show updateB
       , ApplicableEdit (UpdateEdit updateB)
       , FullSubjectReader (UpdateReader updateB)
       , Eq (UpdateSubject updateB)
       , Show (UpdateSubject updateB)
       , Show state
       )
    => FloatingEditLens updateA updateB
    -> UpdateSubject updateA
    -> UpdateEdit updateA
    -> Property
lensUpdateGetProperty lens oldA editA =
    ioProperty @Property $
    runLifeCycle $ do
        MkFloatingEditLens {..} <- return lens
        --oldState <- get
        r <- runFloatInit felInit $ subjectToReadable oldA
        newA <- readableToSubject $ applyEdit editA $ subjectToReadable oldA
        oldB <- readableToSubject $ elGet (felLens r) $ subjectToReadable oldA
        updateBs <- elUpdate (felLens r) (editUpdate editA) $ subjectToReadable newA
        --newState <- get
        newB1 <- readableToSubject $ applyEdits (fmap updateEdit updateBs) $ subjectToReadable oldB
        newB2 <- readableToSubject $ elGet (felLens r) $ subjectToReadable newA
        let
            vars =
                [ showVar "oldA" oldA
                --, showVar "oldState" oldState
                , showVar "oldB" oldB
                , showVar "editA" editA
                , showVar "updateBs" updateBs
                , showVar "newA" newA
                --, showVar "newState" newState
                , showVar "newB (edits)" newB1
                , showVar "newB (lens )" newB2
                ]
        return $ counterexamples vars $ newB1 === newB2

testLensUpdate :: TestTree
testLensUpdate =
    testProperty "update" $ \run (MkSimpleString base) edit ->
        lensUpdateGetProperty @(SequenceRun String) (stringSectionLens run) base edit

testStringSectionLens :: TestTree
testStringSectionLens =
    testGroup
        "string section lens"
        [ testLensGet
        , localOption (QuickCheckTests 10000) testLensUpdate
        , localOption (QuickCheckTests 1) $
          testGroup "update special" $
          [ testProperty "1 0" $
            lensUpdateGetProperty
                @(SequenceRun String)
                (stringSectionLens $ seqRun 0 1)
                "A"
                (StringReplaceSection (seqRun 1 0) "x")
          , testProperty "4 1" $
            lensUpdateGetProperty
                @(SequenceRun String)
                (stringSectionLens $ seqRun 0 5)
                "ABCDE"
                (StringReplaceSection (seqRun 4 1) "pqrstu")
          , testProperty "4 2" $
            lensUpdateGetProperty
                @(SequenceRun String)
                (stringSectionLens $ seqRun 0 5)
                "ABCDE"
                (StringReplaceSection (seqRun 4 2) "pqrstu")
          , testProperty "SharedString5" $
            lensUpdateGetProperty
                @(SequenceRun String)
                (stringSectionLens $ startEndRun @String 1 3)
                "ABCD"
                (StringReplaceSection (startEndRun 2 4) "")
          ]
        ]

testSequence :: TestTree
testSequence =
    testGroup
        "sequence"
        [ testCase "intersectInside" $
          assertEqual "" (Just $ startEndRun @[()] 2 3) $ seqIntersectInside (startEndRun 1 3) (startEndRun 2 4)
        ]

tests :: TestTree
tests =
    testGroup
        "truth-core"
        [ testResource
        , testApplyEditsPar
        , testApplyEditsSeq
        , testSequence
        , testStringEdit
        , testStringSectionLens
        , testSubscribe
        , testLens
        ]

main :: IO ()
main = defaultMain tests
