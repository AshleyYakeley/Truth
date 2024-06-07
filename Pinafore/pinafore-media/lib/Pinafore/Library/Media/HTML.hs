module Pinafore.Library.Media.HTML
    ( HTMLText(..)
    , htmlStuff
    ) where

import Changes.World.Media.Type
import Data.Shim
import Pinafore.API
import Pinafore.Library.Media.Media
import Shapes

newtype HTMLText = MkHTMLText
    { unHTMLText :: Text
    } deriving newtype (Eq, AsTypedLiteral)

instance AsLiteral HTMLText

-- HTMLText
htmlTextGroundType :: QGroundType '[] HTMLText
htmlTextGroundType = mkLiteralGroundType $(iowitness [t|'MkWitKind (SingletonFamily HTMLText)|]) "HTMLText"

instance HasQGroundType '[] HTMLText where
    qGroundType = htmlTextGroundType

htmlAsMedia :: Codec Media HTMLText
htmlAsMedia =
    coerceCodec .
    mediaSpecificText
        (ApplicationMediaType, "html")
        (\case
             (TextMediaType, "html") -> True
             (ApplicationMediaType, "html") -> True
             _ -> False)

htmlStuff :: LibraryStuff ()
htmlStuff =
    headingBDS "HTML" "" $
    [ typeBDS
          "HTMLText"
          "Text that's intended to be HTML (not necessarily valid)."
          (MkSomeGroundType htmlTextGroundType)
          [valPatBDS "Mk" "" MkHTMLText $ PureFunction $ \(MkHTMLText t) -> (t, ())]
    , hasSubtypeRelationBDS @HTMLText @Text Verify "" $ functionToShim "unHTMLText" unHTMLText
    , namespaceBDS "HTMLText" [valBDS "asMedia" "" $ codecToPrism htmlAsMedia]
    ]
