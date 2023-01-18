{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.MIME
    ( mimeLibSection
    ) where

import Pinafore.Base
import Pinafore.Language.Convert.HasType
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Optics ()
import Pinafore.Language.Type
import Pinafore.Language.Value
import Shapes

-- MIME
mimeGroundType :: QGroundType '[] MIME
mimeGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily MIME)|]) "MIME"

instance HasQGroundType '[] MIME where
    qGroundType = mimeGroundType

textMIME :: LangPrism' MIME Text
textMIME = prism mimeToText textToMIME

mimeLibSection :: BindDocTree context
mimeLibSection =
    headingBDT
        "MIME"
        ""
        [ typeBDT "MIME" "" (MkSomeGroundType mimeGroundType) []
        , namespaceBDT "MIME" "" [nameInRootBDT $ valBDT "textMIME" "" textMIME]
        ]
