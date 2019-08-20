{-# OPTIONS -fno-warn-orphans #-}

module Main
    ( main
    ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Constraint
import Control.Monad.Trans.State
import Control.Monad.Trans.Unlift
import Data.Sequences
import Data.Type.Equality
import Prelude
import Subscribe
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Truth.Core
import Unsafe.Coerce

newtype SimpleString = MkSimpleString
    { getSimpleString :: String
    } deriving (Eq)

instance Show SimpleString where
    show (MkSimpleString s) = show s

simplifyChar :: Char -> [Char]
simplifyChar 'A' = []
simplifyChar t
    | t < 'A' = ['A', succ t]
simplifyChar t = ['A', pred t]

simplifyChars :: String -> [String]
simplifyChars [] = []
simplifyChars (c:cc) = let
    rest :: [String]
    rest = fmap ((:) c) $ simplifyChars cc
    changes :: [String]
    changes = fmap (\c' -> c' : cc) $ simplifyChar c
    in changes <> rest

instance Arbitrary SimpleString where
    arbitrary = MkSimpleString . getPrintableString <$> arbitrary
    shrink (MkSimpleString []) = []
    shrink (MkSimpleString s@(_:cc)) = MkSimpleString <$> (cc : simplifyChars s)
      where


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
        edits :: [PairEdit (WholeEdit Bool) (WholeEdit Bool)]
        edits = [MkTupleEdit SelectFirst $ MkWholeEdit True, MkTupleEdit SelectSecond $ MkWholeEdit True]
        expected = (True, True)
        rf :: ReadFunction (TupleEditReader (PairSelector (WholeEdit Bool) (WholeEdit Bool))) (TupleEditReader (PairSelector (WholeEdit Bool) (WholeEdit Bool)))
        rf = applyEdits edits
        in do
               found <- mutableReadToSubject $ rf $ subjectToMutableRead start
               assertEqual "" expected found

testApplyEditsSeq :: TestTree
testApplyEditsSeq =
    testCase "apply edits sequence" $ let
        start = 0
        edits :: [WholeEdit Int]
        edits = [MkWholeEdit 1, MkWholeEdit 2]
        expected = 2
        rf :: ReadFunction (WholeReader Int) (WholeReader Int)
        rf = applyEdits edits
        in do
               found <- mutableReadToSubject $ rf $ subjectToMutableRead start
               assertEqual "" expected found

applyEditSubject ::
       (ApplicableEdit edit, FullSubjectReader (EditReader edit)) => edit -> EditSubject edit -> IO (EditSubject edit)
applyEditSubject edit subj = mutableReadToSubject $ applyEdit edit $ subjectToMutableRead subj

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
           found <- applyEdit edit (subjectToMutableRead original) rt
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
    testProperty "get" $ \run (base :: String) ->
        ioProperty $ do
            MkCloseUnlift (MkUnlift unlift) MkAnEditLens {..} <- stringSectionLens run
            let MkAnEditFunction {..} = elFunction
            unlift $
                withTransConstraintTM @MonadIO $ do
                    let
                        expected :: String
                        expected = subjectToRead base $ StringReadSection run
                    found <- mutableReadToSubject $ efGet $ subjectToMutableRead base
                    return $ found === expected

showVar :: Show a => String -> a -> String
showVar name val = name ++ " = " ++ show val

counterexamples :: [String] -> Property -> Property
counterexamples [] = id
counterexamples (s:ss) = counterexample s . counterexamples ss

unsafeRefl :: forall a b. a :~: b
unsafeRefl = unsafeCoerce Refl

lensUpdateGetProperty ::
       forall state edita editb.
       ( Show edita
       , ApplicableEdit edita
       , FullSubjectReader (EditReader edita)
       , Show (EditSubject edita)
       , Show editb
       , ApplicableEdit editb
       , FullSubjectReader (EditReader editb)
       , Eq (EditSubject editb)
       , Show (EditSubject editb)
       , Show state
       )
    => IO (EditLens edita editb)
    -> EditSubject edita
    -> edita
    -> Property
lensUpdateGetProperty getlens oldA editA =
    ioProperty @Property $ do
        MkCloseUnlift (MkUnlift unlift :: Unlift t) (MkAnEditLens {..}) <- getlens
        case unsafeRefl @t @(StateT state) of
            Refl ->
                unlift $ do
                    let MkAnEditFunction {..} = elFunction
                    editFirst <- get
                    newA <- mutableReadToSubject $ applyEdit editA $ subjectToMutableRead oldA
                    oldB <- mutableReadToSubject $ efGet $ subjectToMutableRead oldA
                    editBs <- efUpdate editA $ subjectToMutableRead newA
                    newState <- get
                    newB1 <- mutableReadToSubject $ applyEdits editBs $ subjectToMutableRead oldB
                    newB2 <- mutableReadToSubject $ efGet $ subjectToMutableRead newA
                    let
                        vars =
                            [ showVar "oldA" oldA
                            , showVar "oldState" editFirst
                            , showVar "oldB" oldB
                            , showVar "editA" editA
                            , showVar "editBs" editBs
                            , showVar "newA" newA
                            , showVar "newState" newState
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
        [testApplyEditsPar, testApplyEditsSeq, testSequence, testStringEdit, testStringSectionLens, testSubscribe]

main :: IO ()
main = defaultMain tests
