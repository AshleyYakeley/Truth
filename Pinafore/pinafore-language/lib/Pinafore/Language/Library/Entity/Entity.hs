module Pinafore.Language.Library.Entity.Entity
    ( entityEntityLibSection
    ) where

import Import
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type

entityAnchor :: Entity -> Text
entityAnchor p = pack $ show p

entityEntityLibSection :: LibraryStuff context
entityEntityLibSection =
    headingBDS
        "Entity"
        ""
        [ typeBDS
              "Entity"
              "Something that can be identified by a 256-bit anchor."
              (MkSomeGroundType entityGroundType)
              []
        , namespaceBDS "Entity" $
          fmap addNameInRootBDS (eqEntries @_ @Entity) <>
          [ valBDS "order" "An arbitrary order on `Entity`." $ compare @Entity
          , valBDS "anchor" "The anchor of an entity, as text." entityAnchor
          ]
        ]
