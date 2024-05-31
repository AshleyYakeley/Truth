{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.MIME
    ( mimeLibSection
    ) where

import Import
import Pinafore.Language.Convert.HasType
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Optics ()
import Pinafore.Language.Type
import Pinafore.Language.Value

-- MIME
mimeGroundType :: QGroundType '[] MIME
mimeGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily MIME)|]) "MIME"

instance HasQGroundType '[] MIME where
    qGroundType = mimeGroundType

textMIME :: LangPrism' MIME Text
textMIME = prism mimeToText textToMIME

mimeLibSection :: LibraryStuff context
mimeLibSection =
    headingBDS
        "MIME"
        ""
        [typeBDS "MIME" "" (MkSomeGroundType mimeGroundType) [], namespaceBDS "MIME" [valBDS "text" "" textMIME]]
