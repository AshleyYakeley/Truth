module Pinafore.Language.Library.Entity.Unit
    ( unitEntityLibSection
    )
where

import Import
import Pinafore.Language.Convert.Types
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Literal
import Pinafore.Language.Library.Entity.Showable
import Pinafore.Language.Library.LibraryModule

unitEntityLibSection :: LibraryStuff
unitEntityLibSection =
    headingBDS
        "Unit"
        ""
        [ typeBDS "Unit" "A type with one value, `()`." (MkSomeGroundType unitGroundType) []
        , literalSubtypeRelationEntry @()
        , showableSubtypeRelationEntry @()
        , namespaceBDS "Unit" $ monoidEntries @() <> eqEntries @()
        ]
