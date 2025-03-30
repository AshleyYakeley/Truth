module Pinafore.Language.Library.Entity.Void
    ( voidEntityLibSection
    )
where

import Import
import Pinafore.Language.Convert.Types
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Literal
import Pinafore.Language.Library.Entity.Showable
import Pinafore.Language.Library.LibraryModule

voidEntityLibSection :: LibraryStuff
voidEntityLibSection =
    headingBDS
        "Void"
        ""
        [ typeBDS "Void" "A type with no values." (MkSomeGroundType voidGroundType) []
        , literalSubtypeRelationEntry @Void
        , showableSubtypeRelationEntry @Void
        , namespaceBDS "Void"
            $ semigroupEntries @Void
            <> eqEntries @Void
            <> [ addNameInRootBDS $ valBDS "absurd" "" (absurd :: Void -> BottomType)
               ]
        ]
