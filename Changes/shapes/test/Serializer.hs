module Serializer
    ( testSerializer
    )
where

import Shapes
import Shapes.Test

checkSerializer :: Eq a => Serializer stp a -> a -> Bool
checkSerializer s value = let
    bs = serializerStrictEncode s value
    found = serializerStrictDecode s bs
    in found == Just value

test ::
    forall a.
    (HasSerializer a, Eq a, Show a, Arbitrary a) =>
    String ->
    TestTree
test name =
    testTree
        name
        [ testTree "stopping" $ checkSerializer $ stoppingSerializer @a
        , testTree "greedy" $ checkSerializer $ greedySerializer @a
        ]

testSerializer :: TestTree
testSerializer =
    testTree
        "serializer"
        [ test @Integer "Integer"
        , test @String "String"
        , test @Int32 "Int32"
        , test @StrictByteString "StrictByteString"
        , test @(Int32, Int32) "(Int32,Int32)"
        , test @(StrictByteString, StrictByteString) "(StrictByteString,StrictByteString)"
        , test @[Bool] "[Bool]"
        , test @[Word16] "[Word16]"
        , test @[[Word16]] "[[Word16]]"
        , test @[StrictByteString] "[StrictByteString]"
        , test @[[StrictByteString]] "[[StrictByteString]]"
        ]
