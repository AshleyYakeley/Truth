module Shim
    ( testShim
    ) where

import Data.Shim
import Language.Expression.Common
import Shapes
import Test.Tasty
import Test.Tasty.HUnit

type T = UVar Type "t"

type REC = T :~: Maybe T

conv1 ::
       forall shim a b. Category shim
    => a :~: b -> shim a b
conv1 Refl = id

conv2 ::
       forall shim a b. Category shim
    => a :~: b -> shim b a
conv2 Refl = id

convT1 ::
       forall shim. Category shim
    => REC
    -> shim T (Maybe T)
convT1 = conv1 @shim @T @(Maybe T)

convT2 ::
       forall shim. Category shim
    => REC
    -> shim (Maybe T) T
convT2 = conv2 @shim @T @(Maybe T)

nothingRec :: REC -> T
nothingRec r = convT2 r Nothing

justRec :: REC -> T -> T
justRec r x = convT2 r $ Just x

countRec :: REC -> T -> Int
countRec r t =
    case convT1 r t of
        Nothing -> 0
        Just t' -> succ $ countRec r t'

withRec' :: forall t r. (T :~: t -> r) -> r
withRec' call = assignUVar @Type @t (MkSymbolType @"t") $ call Refl

withRec :: forall r. (REC -> r) -> r
withRec = withRec' @(Maybe T)

testRec :: TestTree
testRec =
    testCase "rec" $
    withRec $ \r -> let
        r3 = justRec r $ justRec r $ justRec r $ nothingRec r
        found = countRec r r3
        in assertEqual "" 3 found

justJM :: REC -> JMShim Type T T
justJM r = convT2 r . toEnhanced "J" Just

endless1 :: REC -> JMShim Type T T
endless1 r = justJM r . lazyEnhanced (endless1 r)

endless2 :: REC -> JMShim Type T T
endless2 r = lazyEnhanced (endless1 r) . justJM r

applyEndless1 :: REC -> T
applyEndless1 r = fromEnhanced (endless1 r) $ nothingRec r

applyEndless2 :: REC -> T
applyEndless2 r = fromEnhanced (endless2 r) $ nothingRec r

checkEndless :: REC -> Int -> T -> Bool
checkEndless _ 0 _ = True
checkEndless r n t =
    case convT1 r t of
        Just _ -> checkEndless r (pred n) t
        Nothing -> False

testEndless1 :: TestTree
testEndless1 = testCase "endless1" $ withRec $ \r -> assertEqual "" True $ checkEndless r 17 $ applyEndless1 r

testEndless2 :: TestTree
testEndless2 = testCase "endless2" $ withRec $ \r -> assertEqual "" True $ checkEndless r 17 $ applyEndless2 r

testShim :: TestTree
testShim = testGroup "shim" [testRec, testEndless1, testEndless2]
