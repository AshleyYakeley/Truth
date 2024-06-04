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

blobEntityLibSection :: LibraryStuff context
blobEntityLibSection =
    headingBDS
        "Blob"
        ""
        [ typeBDS "Blob" "" (MkSomeGroundType blobGroundType) []
        , literalSubtypeRelationEntry @StrictByteString
        , showableSubtypeRelationEntry @StrictByteString
        , namespaceBDS "Blob" $
          mconcat
              [ monoidEntries @_ @StrictByteString
              , orderEntries (compare @StrictByteString) ""
              , sequenceEntries @_ @StrictByteString
              ]
        ]
