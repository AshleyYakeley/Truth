{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Entity.Blob
    ( blobEntityLibSection
    ) where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Literal
import Pinafore.Language.Library.Entity.Showable
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type

blobGroundType :: QGroundType '[] StrictByteString
blobGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily StrictByteString)|]) "Blob"

instance HasQGroundType '[] StrictByteString where
    qGroundType = blobGroundType

toBlob :: [Integer] -> StrictByteString
toBlob ii = fromList $ fmap fromInteger ii

fromBlob :: StrictByteString -> [Integer]
fromBlob b = fmap toInteger $ otoList b

blobEntityLibSection :: LibraryStuff context
blobEntityLibSection =
    headingBDS
        "Blob"
        "A sequence of bytes."
        [ typeBDS
              "Blob"
              ""
              (MkSomeGroundType blobGroundType)
              [valPatBDS "Mk" "As a list of bytes." toBlob $ PureFunction $ \b -> (fromBlob b, ())]
        , literalSubtypeRelationEntry @StrictByteString
        , showableSubtypeRelationEntry @StrictByteString
        , namespaceBDS "Blob" $
          mconcat
              [ monoidEntries @_ @StrictByteString
              , orderEntries (compare @StrictByteString) ""
              , sequenceEntries @_ @StrictByteString
              ]
        ]
