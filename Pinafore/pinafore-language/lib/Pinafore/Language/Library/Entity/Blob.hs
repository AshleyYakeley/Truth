{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Entity.Blob
    ( blobEntityLibSection
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Literal
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Optics ()
import Pinafore.Language.Library.Showable
import Pinafore.Language.Type
import Pinafore.Language.Value

blobGroundType :: QGroundType '[] StrictByteString
blobGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily StrictByteString)|]) "Blob"

instance HasQGroundType '[] StrictByteString where
    qGroundType = blobGroundType

toBlob :: [Word8] -> StrictByteString
toBlob = fromList

fromBlob :: StrictByteString -> [Word8]
fromBlob = otoList

hexTextPrism :: LangPrism' Text StrictByteString
hexTextPrism = prism (fromLooseHexadecimal . unpack) (pack . toHexadecimal)

showHex :: StrictByteString -> Text
showHex = pack . toLowercaseHexadecimal

blobEntityLibSection :: LibraryStuff
blobEntityLibSection =
    headingBDS
        "Blob"
        "A sequence of bytes."
        [ typeBDS
            "Blob"
            ""
            (MkSomeGroundType blobGroundType)
            [valPatBDS "Mk" "As a list of bytes." toBlob $ PureFunction $ pure $ \b -> (fromBlob b, ())]
        , literalSubtypeRelationEntry @StrictByteString
        , showableSubtypeRelationEntry "show as lower-case hexadecimal" showHex
        , namespaceBDS "Blob"
            $ mconcat
                [ monoidEntries @StrictByteString
                , orderEntries (ordOrder @StrictByteString) ""
                , sequenceEntries @StrictByteString
                ,
                    [ valBDS
                        "asHexText"
                        "Represent a `Blob` as hexadecimal `Text`. Encodes as upper-case, decodes case insensitively and ignores spaces and punctuation, but fails on non-hex letters."
                        hexTextPrism
                    ]
                ]
        ]
