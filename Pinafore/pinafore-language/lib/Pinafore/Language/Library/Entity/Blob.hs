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
        , namespaceBDS "Text" $
          monoidEntries @_ @StrictByteString <>
          orderEntries (compare @StrictByteString) "" <>
          [ valBDS "length" "The length of a blob." $ olength @StrictByteString
          , valBDS "section" "`section start len blob` is the section of `blob` beginning at `start` of length `len`." $ \start len (x :: StrictByteString) ->
                take len $ drop start x
          ]
        ]
