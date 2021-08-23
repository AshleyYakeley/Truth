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
        assertEqual "length" 32 $ olength bs

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
    assertEqual "show" "!6BC56234-BED17C8C-C7C1AD4A-4988232B-9ADDB290-E640C610-74933DDC-5C28ABA2" $
    show $ hashToAnchor $ \call -> [call @Text "anchor test"]

testAnchor :: TestTree
testAnchor = testTree "anchor" [testLength, testEncodeDecode, testShow]
