{-# OPTIONS -fno-warn-orphans #-}

module Main
    ( main
    ) where

import Changes.Core
import Lens
import List
import Resource
import Shapes
import Shapes.Test
import Subscribe
import Test.SimpleString

instance Arbitrary SequencePoint where
    arbitrary = MkSequencePoint <$> (getSmall . getNonNegative <$> arbitrary)
    shrink 0 = []
    shrink 1 = [0]
    shrink n = [0, pred n]

instance Arbitrary SequenceRun where
    arbitrary = MkSequenceRun <$> arbitrary <*> arbitrary
    shrink (MkSequenceRun s l) = [MkSequenceRun s' l' | (s', l') <- shrink (s, l)]

instance forall seq. Arbitrary seq => Arbitrary (StringEdit seq) where
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
    testTree "apply edits parallel" $ let
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
    testTree "apply edits sequence" $ let
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
       forall edit.
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
    in testTree name $ do
           found <- applyEditSubject edit original
           assertEqual "" expected found

testEditRead ::
       forall edit t.
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
    in testTree name $ do
           found <- applyEdit edit (subjectToReadable original) rt
           assertEqual "" expected found

seqRun :: Int64 -> Int64 -> SequenceRun
seqRun start len = MkSequenceRun (MkSequencePoint start) (MkSequencePoint len)

testStringEdit :: TestTree
testStringEdit =
    testTree
        "string edit"
        [ testEdit (StringReplaceSection @String (seqRun 2 0) "XY") "ABCDE" "ABXYCDE"
        , testEdit (StringReplaceSection @String (seqRun 2 1) "XY") "ABCDE" "ABXYDE"
        , testEdit (StringReplaceSection @String (seqRun 2 2) "XY") "ABCDE" "ABXYE"
        , testEdit (StringReplaceSection @String (seqRun 2 3) "XY") "ABCDE" "ABXY"
        , testEdit (StringReplaceSection @String (seqRun 1 3) "XY") "ABCDE" "AXYE"
        , testEditRead (StringReplaceSection @String (seqRun 2 0) "XY") "ABCDE" StringReadLength (MkSequencePoint 7)
        , testEditRead
              (StringReplaceSection @String (seqRun 2 0) "XY")
              "ABCDE"
              (StringReadSection $ seqRun 0 7)
              "ABXYCDE"
        , testEditRead (StringReplaceSection @String (seqRun 2 0) "XY") "ABCDE" (StringReadSection $ seqRun 0 3) "ABX"
        , testEditRead (StringReplaceSection @String (seqRun 2 0) "XY") "ABCDE" (StringReadSection $ seqRun 1 3) "BXY"
        , testEditRead (StringReplaceSection @String (seqRun 2 0) "XY") "ABCDE" (StringReadSection $ seqRun 1 4) "BXYC"
        , testEditRead (StringReplaceSection @String (seqRun 2 0) "XY") "ABCDE" (StringReadSection $ seqRun 2 4) "XYCD"
        , testEditRead (StringReplaceSection @String (seqRun 2 0) "XY") "ABCDE" (StringReadSection $ seqRun 3 3) "YCD"
        ]

testLensGet :: TestTree
testLensGet =
    testTree "get" $ \srun (base :: String) ->
        ioProperty $
        runLifecycle $ do
            MkFloatingChangeLens {..} <- return $ stringSectionLens srun
            r <- runFloatInit fclInit $ subjectToReadable base
            let
                expected :: String
                expected = subjectToRead base $ StringReadSection srun
            found <- readableToSubject $ clRead (fclLens r) $ subjectToReadable base
            return $ found === expected

showVar :: Show a => String -> a -> String
showVar name val = name ++ " = " ++ show val

counterexamples :: [String] -> Property -> Property
counterexamples [] = id
counterexamples (s:ss) = counterexample s . counterexamples ss

lensUpdateGetProperty ::
       forall updateA updateB.
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
       )
    => FloatingChangeLens updateA updateB
    -> UpdateSubject updateA
    -> UpdateEdit updateA
    -> Property
lensUpdateGetProperty lens oldA editA =
    ioProperty @Property $
    runLifecycle $ do
        MkFloatingChangeLens {..} <- return lens
        --oldState <- get
        r <- runFloatInit fclInit $ subjectToReadable oldA
        newA <- readableToSubject $ applyEdit editA $ subjectToReadable oldA
        oldB <- readableToSubject $ clRead (fclLens r) $ subjectToReadable oldA
        updateBs <- clUpdate (fclLens r) (editUpdate editA) $ subjectToReadable newA
        --newState <- get
        newB1 <- readableToSubject $ applyEdits (fmap updateEdit updateBs) $ subjectToReadable oldB
        newB2 <- readableToSubject $ clRead (fclLens r) $ subjectToReadable newA
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
    testTree "update" $ \run (MkSimpleString base) edit -> lensUpdateGetProperty (stringSectionLens run) base edit

testStringSectionLens :: TestTree
testStringSectionLens =
    testTree
        "string section lens"
        [ testLensGet
        , localOption (QuickCheckTests 10000) testLensUpdate
        , localOption (QuickCheckTests 1) $
          testTree "update special" $
          [ testTree "1 0" $
            lensUpdateGetProperty
                (stringSectionLens $ seqRun 0 1)
                ("A" :: String)
                (StringReplaceSection (seqRun 1 0) "x")
          , testTree "4 1" $
            lensUpdateGetProperty
                (stringSectionLens $ seqRun 0 5)
                ("ABCDE" :: String)
                (StringReplaceSection (seqRun 4 1) "pqrstu")
          , testTree "4 2" $
            lensUpdateGetProperty
                (stringSectionLens $ seqRun 0 5)
                ("ABCDE" :: String)
                (StringReplaceSection (seqRun 4 2) "pqrstu")
          , testTree "SharedString5" $
            lensUpdateGetProperty
                (stringSectionLens $ startEndRun 1 3)
                ("ABCD" :: String)
                (StringReplaceSection (startEndRun 2 4) "")
          ]
        ]

testSequence :: TestTree
testSequence =
    testTree
        "sequence"
        [ testTree "intersectInside" $
          assertEqual "" (Just $ startEndRun 2 3) $ seqIntersectInside (startEndRun 1 3) (startEndRun 2 4)
        ]

tests :: TestTree
tests =
    testTree
        "changes-core"
        [ testResource
        , testApplyEditsPar
        , testApplyEditsSeq
        , testSequence
        , testStringEdit
        , testStringSectionLens
        , testSubscribe
        , testLens
        , testList
        ]

main :: IO ()
main = testMain tests
