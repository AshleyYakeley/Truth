module Test.ReadShow
    ( testReadShow
    )
where

import Pinafore.Base
import Shapes
import Shapes.Test

import Pinafore.Syntax

testShowInteger :: String -> Integer -> TestTree
testShowInteger expected i = testTree expected $ assertEqual "" expected $ rsShow integerReadShow i

testItem :: String -> String -> TestTree
testItem expected text =
    testTree text $ let
        mfound :: Maybe SafeRational
        mfound = readLiteralMaybe text
        in case mfound of
            Nothing -> assertFailure "no parse"
            Just found -> assertEqual "" expected $ show found

testReadShow :: TestTree
testReadShow =
    testTree
        "read-show"
        [ testTree
            "Integer"
            [ testShowInteger "0" 0
            , testShowInteger "-1" $ negate 1
            , testShowInteger "354" 354
            , testShowInteger "-5812" $ negate 5812
            ]
        , testTree
            "SafeRational"
            [ testItem "0" "0"
            , testItem "0" "-0"
            , testItem "0" "0.0"
            , testItem "0" "-0.0"
            , testItem "1" "1"
            , testItem "-1" "-1"
            , testItem "-17" "-17"
            , testItem "7/2" "3.5"
            , testItem "7/2" "7/2"
            , testItem "5/3" "5/3"
            , testItem "-5/3" "-5/3"
            , testItem "5/3" "1._6"
            , testItem "-5/3" "-1._6"
            , testItem "5/3" "1.6_6"
            , testItem "5/3" "1.6_66"
            , testItem "5/3" "1._66"
            , testItem "2/3" "0._66"
            , testItem "-2/3" "-0._66"
            , testItem "2/5" "0.4"
            , testItem "-2/5" "-0.4"
            ]
        ]
