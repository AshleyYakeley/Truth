{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Media
    ( mediaLibSection
    ) where

import Import
import Pinafore.Language.Convert.HasType
import Pinafore.Language.Convert.Literal
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Literal
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Optics ()
import Pinafore.Language.Type
import Pinafore.Language.Value

-- Media
mediaGroundType :: QGroundType '[] Media
mediaGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily Media)|]) "Media"

instance HasQGroundType '[] Media where
    qGroundType = mediaGroundType

textMedia :: LangPrism' Media Text
textMedia = codecToPrism mediaText

mediaLibSection :: LibraryStuff context
mediaLibSection =
    headingBDS
        "Media"
        ""
        [ typeBDS "Media" "" (MkSomeGroundType mediaGroundType) []
        , literalSubtypeRelationEntry @Media
        , namespaceBDS "Media" [valBDS "text" "" textMedia]
        ]
