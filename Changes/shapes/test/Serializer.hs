module Serializer
    ( testSerializer
    ) where

import Shapes
import Shapes.Test

test ::
       forall a. (HasSerializer a, Eq a, Show a, Arbitrary a)
    => String
    -> TestTree
test name =
    testTree name $ \value -> let
        s :: forall . Serializer a
        s = serializer
        bs = serializerStrictEncode s value
        found = serializerStrictDecode s bs
        in found == Just value

testSerializer :: TestTree
testSerializer = testTree "serializer" [test @Integer "Integer", test @String "String", test @Int32 "Int32"]
