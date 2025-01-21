module Test.Anchor
    ( testAnchor
    )
where

import Shapes
import Shapes.Test

import Pinafore.Base

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
    testTree "show"
        $ assertEqual "show" "!16E443B7-24DCA0C9-553E2859-DFCBB0C1-6BE36683-48C4DB84-B26029C6-529A9E02"
        $ show
        $ hashToAnchor
        $ \call -> [call @Text "anchor test"]

testAnchor :: TestTree
testAnchor = testTree "anchor" [testLength, testEncodeDecode, testShow]
