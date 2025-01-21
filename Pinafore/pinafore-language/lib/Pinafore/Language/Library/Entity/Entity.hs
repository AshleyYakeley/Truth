module Pinafore.Language.Library.Entity.Entity
    ( entityEntityLibSection
    )
where

import Import
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Blob ()
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Optics ()
import Pinafore.Language.Type
import Pinafore.Language.Value

entityAnchor :: Entity -> Text
entityAnchor p = pack $ show p

blobPrism :: LangPrism' StrictByteString Entity
blobPrism = codecToPrism $ MkCodec (Just . MkEntity) (\(MkEntity a) -> a) . anchorCodec

entityEntityLibSection :: LibraryStuff
entityEntityLibSection =
    headingBDS
        "Entity"
        ""
        [ typeBDS
            "Entity"
            "Something that can be identified by a 256-bit anchor."
            (MkSomeGroundType entityGroundType)
            []
        , namespaceBDS "Entity"
            $ fmap addNameInRootBDS (eqEntries @Entity)
            <> [ valBDS "order" "An arbitrary order on `Entity`." $ ordOrder @Entity
               , valBDS "anchor" "The anchor of an entity, as text." entityAnchor
               , valBDS "asBlob" "Represent an `Entity` as a `Blob`." blobPrism
               ]
        ]
