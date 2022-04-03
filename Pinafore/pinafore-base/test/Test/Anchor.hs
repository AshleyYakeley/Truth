module Test.Anchor
    ( testAnchor
    ) where

import Pinafore.Base
import Shapes
import Shapes.Test

testLength :: TestTree
testLength =
    testTree "length" $ do
        anchor :: Anchor <- randomIO
        let bs = encodeM anchorCodec anchor
        assertEqual "length" (hashSize @Anchor) $ olength bs

testEncodeDecode :: TestTree
testEncodeDecode =
    testTree "encode-decode" $ do
        anchor :: Anchor <- randomIO
        let bs = encodeM anchorCodec anchor
        anchor' <- decode anchorCodec bs
        assertEqual "anchor" anchor anchor'

testShow :: TestTree
testShow =
    testTree "show" $
    assertEqual "show" "!1E900FD8-BDE7A133-A047B86B-C8C74129-8B69FD90-6E097C9B-9A579187-8A3830C8" $
    show $ hashToAnchor $ \call -> [call @Text "anchor test"]

testAnchor :: TestTree
testAnchor = testTree "anchor" [testLength, testEncodeDecode, testShow]
