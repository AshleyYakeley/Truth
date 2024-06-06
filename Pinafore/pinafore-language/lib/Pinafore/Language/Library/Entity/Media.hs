{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Entity.Media
    ( mediaEntityLibSection
    ) where

import Changes.World.Media.Type
import Import
import Pinafore.Language.Convert.HasType
import Pinafore.Language.Convert.Literal
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Blob ()
import Pinafore.Language.Library.Entity.Literal
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Optics ()
import Pinafore.Language.Type
import Pinafore.Language.Value

-- MediaType
mediaTypeGroundType :: QGroundType '[] MediaType
mediaTypeGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily MediaType)|]) "Type.Media."

instance HasQGroundType '[] MediaType where
    qGroundType = mediaTypeGroundType

-- Media
mediaGroundType :: QGroundType '[] Media
mediaGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily Media)|]) "Media"

instance HasQGroundType '[] Media where
    qGroundType = mediaGroundType

textMedia :: LangPrism' Media Text
textMedia = codecToPrism mediaText

mediaEntityLibSection :: LibraryStuff context
mediaEntityLibSection =
    headingBDS
        "Media"
        ""
        [ namespaceBDS
              "Media"
              [ typeBDS
                    "Type"
                    "RFC 6838 media type."
                    (MkSomeGroundType mediaTypeGroundType)
                    [ valPatBDS "Mk" "Type, subtype, parameters" MkMediaType $
                      PureFunction $ \(MkMediaType t s p) -> (t, (s, (p, ())))
                    ]
              , literalSubtypeRelationEntry @MediaType
              ]
        , typeBDS
              "Media"
              "A blob and an RFC 6838 media type that interprets it."
              (MkSomeGroundType mediaGroundType)
              [valPatBDS "Mk" "Type and Blob" MkMedia $ PureFunction $ \(MkMedia t b) -> (t, (b, ()))]
        , literalSubtypeRelationEntry @Media
        , namespaceBDS "Media" [valBDS "text" "" textMedia]
        ]
