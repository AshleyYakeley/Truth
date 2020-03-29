module Test.Anchor
    ( testAnchor
    ) where

import Pinafore.Base
import Shapes
import Test.Tasty
import Test.Tasty.HUnit

testLength :: TestTree
testLength =
    testCase "length" $ do
        anchor :: Anchor <- randomIO
        let bs = encodeM anchorCodec anchor
        assertEqual "length" 32 $ olength bs

testEncodeDecode :: TestTree
testEncodeDecode =
    testCase "encode-decode" $ do
        anchor :: Anchor <- randomIO
        let bs = encodeM anchorCodec anchor
        anchor' <- decode anchorCodec bs
        assertEqual "anchor" anchor anchor'

testAnchor :: TestTree
testAnchor = testGroup "anchor" [testLength, testEncodeDecode]
