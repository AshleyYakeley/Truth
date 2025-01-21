module Shim
    ( testShim
    )
where

import Data.Shim
import Shapes
import Shapes.Test

import Language.Expression.TypeSystem

type T = UVarT "t"

type REC = T :~: Maybe T

conv1 ::
    forall (shim :: ShimKind Type) a b.
    Category shim =>
    a :~: b -> shim a b
conv1 Refl = id

conv2 ::
    forall (shim :: ShimKind Type) a b.
    Category shim =>
    a :~: b -> shim b a
conv2 Refl = id

convT1 ::
    forall (shim :: ShimKind Type).
    Category shim =>
    REC ->
    shim T (Maybe T)
convT1 = conv1 @shim @T @(Maybe T)

convT2 ::
    forall (shim :: ShimKind Type).
    Category shim =>
    REC ->
    shim (Maybe T) T
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
withRec' call = assignUVarT @t (MkSymbolType @"t") $ call Refl

withRec :: forall r. (REC -> r) -> r
withRec = withRec' @(Maybe T)

testRec :: TestTree
testRec =
    testTree "rec"
        $ withRec
        $ \r -> let
            r3 = justRec r $ justRec r $ justRec r $ nothingRec r
            found = countRec r r3
            in assertEqual "" 3 found

justJM :: FunctionShim shim => REC -> shim T T
justJM r = convT2 r . functionToShim "J" Just

endless1 ::
    forall (shim :: ShimKind Type).
    (LazyCategory shim, FunctionShim shim) =>
    REC ->
    shim T T
endless1 r = justJM r . iLazy (endless1 r)

endless2 ::
    forall (shim :: ShimKind Type).
    (LazyCategory shim, FunctionShim shim) =>
    REC ->
    shim T T
endless2 r = iLazy (endless1 @shim r) . justJM r

applyEndless1 ::
    forall (shim :: ShimKind Type).
    (LazyCategory shim, RecoverShim shim) =>
    REC ->
    T
applyEndless1 r = shimToFunction (endless1 @shim r) $ nothingRec r

applyEndless2 ::
    forall (shim :: ShimKind Type).
    (LazyCategory shim, RecoverShim shim) =>
    REC ->
    T
applyEndless2 r = shimToFunction (endless2 @shim r) $ nothingRec r

checkEndless :: REC -> Int -> T -> Bool
checkEndless _ 0 _ = True
checkEndless r n t =
    case convT1 r t of
        Just _ -> checkEndless r (pred n) t
        Nothing -> False

testEndless1 :: TestTree
testEndless1 =
    testTree "endless1" $ withRec $ \r -> assertEqual "" True $ checkEndless r 17 $ applyEndless1 @(JMShim Type) r

testEndless2 :: TestTree
testEndless2 =
    testTree "endless2" $ withRec $ \r -> assertEqual "" True $ checkEndless r 17 $ applyEndless2 @(JMShim Type) r

testShim :: TestTree
testShim = testTree "shim" [testRec, testEndless1, testEndless2]
