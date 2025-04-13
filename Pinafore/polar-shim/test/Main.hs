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

testJMPolyShim :: TestTree
testJMPolyShim =
    testTree
        "JMPolyShim"
        [ showTest "id" $ id @(JMPolyShim Type)
        , showTest "[func A.B]" $ func "A" . func "B"
        , showTest "[func A]" $ func "A" . id
        , showTest "initf" $ termf . initf
        , showTest "(joinf [func A] [func B])" $ joinf (func "A") (func "B")
        , showTest "[func A]" $ joinf (func "A") (func "B") . join1
        , expectFailBecause "ISSUE #337" $ showTest "[func A]" $ func "A" . coercionToShim id
        , expectFailBecause "ISSUE #337" $ showTest "[func A]" $ coercionToShim id . func "A"
        ]

tests :: TestTree
tests =
    testTree
        "polar-shim"
        [ testJMPolyShim
        ]

main :: IO ()
main = testMain tests
