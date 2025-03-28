module Pinafore.Language.Library.Entity.Literal
    ( literalSubtypeRelationEntry
    , literalEntityLibSection
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type

literalSubtypeRelationEntry ::
    forall a.
    (HasQType QPolyShim 'Negative a, AsLiteral a) =>
    LibraryStuff
literalSubtypeRelationEntry = hasSubtypeRelationBDS @a @Literal Verify "" $ functionToShim "toLiteral" toLiteral

literalEntityLibSection :: LibraryStuff
literalEntityLibSection =
    headingBDS
        "Literal"
        ""
        [ typeBDS "Literal" "Something that can be represented as a byte list." (MkSomeGroundType literalGroundType) []
        , hasSubtypeRelationBDS @Literal @Entity Verify "Hash with BLAKE3."
            $ functionToShim "literalToEntity" literalToEntity
        ]
