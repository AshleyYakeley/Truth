module Main
    ( main
    )
where

import Shapes
import Shapes.Test

import Data.Shim

showTest :: forall a b. String -> JMPolyShim Type a b -> TestTree
showTest expected shim = testTree expected $ assertEqual "shim" expected $ show shim

func :: forall k a b. CoercibleKind k => String -> JMPolyShim k a b
func name = functionToShim name $ error name

{-

join1 :: a -> JoinType a b
joinf :: (a -> r) -> (b -> r) -> (JoinType a b -> r)

joinf (func "A") (func "B") :: JoinType a b -> r

-}

testJMPolyShim :: TestTree
testJMPolyShim =
    testTree
        "JMPolyShim"
        [ showTest "id" $ id @(JMPolyShim Type)
        , showTest "[A.B]" $ func "A" . func "B"
        , showTest "[A]" $ func "A" . id
        , showTest "initf" $ termf . initf
        , showTest "(joinf [A] [B])" $ joinf (func "A") (func "B")
        , showTest "[A]" $ joinf (func "A") (func "B") . join1
        , showTest "[A]" $ func "A" . coercionToShim id
        , showTest "[A]" $ coercionToShim id . func "A"
        , showTest "[A.C]" $ joinf (func "A") (func "B") . (join1 . func "C")
        , showTest "[A.C]" $ (joinf (func "A") (func "B") . join1) . func "C"
        , expectFailBecause "ISSUE #337" $ showTest "[A]" $ joinf (func "A") (func "B") . (coercionToShim id . join1)
        , expectFailBecause "ISSUE #337" $ showTest "[A]" $ (joinf (func "A") (func "B") . coercionToShim id) . join1
        ]

tests :: TestTree
tests =
    testTree
        "polar-shim"
        [ testJMPolyShim
        ]

main :: IO ()
main = testMain tests
