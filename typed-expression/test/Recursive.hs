module Recursive
    ( testRecursive
    ) where

import Language.Expression.Common
import Language.Expression.Dolan
import Shapes
import Test.Tasty
import Test.Tasty.HUnit

type RecursiveF f = Recursive ('TFConstructor f)

rollRecursiveF ::
       forall f. Functor f
    => f (RecursiveF f)
    -> RecursiveF f
rollRecursiveF = rollRecursive @('TFConstructor f) $ MkApplyFunctor fmap

unrollRecursiveF ::
       forall f. Functor f
    => RecursiveF f
    -> f (RecursiveF f)
unrollRecursiveF = unrollRecursive @('TFConstructor f) $ MkApplyFunctor fmap

countMaybeF :: RecursiveF Maybe -> Int
countMaybeF rm =
    case unrollRecursiveF rm of
        Just rm' -> succ $ countMaybeF rm'
        Nothing -> 0

zeroMaybeF :: RecursiveF Maybe
zeroMaybeF = rollRecursiveF Nothing

succMaybeF :: RecursiveF Maybe -> RecursiveF Maybe
succMaybeF rm = rollRecursiveF $ Just rm

infiniteMaybeF :: RecursiveF Maybe
infiniteMaybeF = succMaybeF infiniteMaybeF

testCount5 :: TestTree
testCount5 =
    testCase "count5" $ let
        r5 :: RecursiveF Maybe
        r5 = succMaybeF $ succMaybeF $ succMaybeF $ succMaybeF $ succMaybeF zeroMaybeF
        in assertEqual "" 5 $ countMaybeF r5

testInfinite :: TestTree
testInfinite = testCase "infinite" $ assertEqual "" True $ isJust $ unrollRecursiveF infiniteMaybeF

testRecursive :: TestTree
testRecursive = testGroup "recursive" [testCount5, testInfinite]
