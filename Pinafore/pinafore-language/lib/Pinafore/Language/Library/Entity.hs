module Pinafore.Language.Library.Entity
    ( entityLibSection
    , showableSubtypeRelationEntry
    , literalSubtypeRelationEntry
    )
where

import Import
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Blob
import Pinafore.Language.Library.Entity.Boolean
import Pinafore.Language.Library.Entity.Entity
import Pinafore.Language.Library.Entity.Literal
import Pinafore.Language.Library.Entity.Numeric
import Pinafore.Language.Library.Entity.Open
import Pinafore.Language.Library.Entity.Ordering
import Pinafore.Language.Library.Entity.Showable
import Pinafore.Language.Library.Entity.Text
import Pinafore.Language.Library.Entity.Time
import Pinafore.Language.Library.Entity.Unit
import Pinafore.Language.Library.Entity.Void
import Pinafore.Language.Library.LibraryModule

entityLibSection :: LibraryStuff
entityLibSection =
    headingBDS "Literals & Entities" ""
        $ [ entityEntityLibSection
          , literalEntityLibSection
          , showableEntityLibSection
          , voidEntityLibSection
          , unitEntityLibSection
          , booleanEntityLibSection
          , orderingEntityLibSection
          , blobEntityLibSection
          , textEntityLibSection
          , numericEntityLibSection
          , timeEntityLibSection
          , openEntityLibSection
          ]
