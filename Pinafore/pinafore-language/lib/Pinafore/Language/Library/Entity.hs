module Pinafore.Language.Library.Entity
    ( entityLibSection
    , showableSubtypeRelationEntry
    , literalSubtypeRelationEntry
    ) where

import Import
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Boolean
import Pinafore.Language.Library.Entity.Dynamic
import Pinafore.Language.Library.Entity.DynamicType
import Pinafore.Language.Library.Entity.Entity
import Pinafore.Language.Library.Entity.Literal
import Pinafore.Language.Library.Entity.Numeric
import Pinafore.Language.Library.Entity.Open
import Pinafore.Language.Library.Entity.Order
import Pinafore.Language.Library.Entity.Ordering
import Pinafore.Language.Library.Entity.Showable
import Pinafore.Language.Library.Entity.Text
import Pinafore.Language.Library.Entity.Time
import Pinafore.Language.Library.Entity.Unit
import Pinafore.Language.Library.LibraryModule

entityLibSection :: LibraryStuff context
entityLibSection =
    headingBDS "Literals & Entities" "" $
    [ entityEntityLibSection
    , literalEntityLibSection
    , showableEntityLibSection
    , unitEntityLibSection
    , booleanEntityLibSection
    , orderingEntityLibSection
    , orderEntityLibSection
    , textEntityLibSection
    , numericEntityLibSection
    , timeEntityLibSection
    , openEntityLibSection
    , dynamicEntityLibSection
    , dynamicTypeEntityLibSection
    ]
