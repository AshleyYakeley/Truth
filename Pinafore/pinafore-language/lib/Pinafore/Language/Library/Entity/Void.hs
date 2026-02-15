module Pinafore.Language.Library.Entity.Void
    ( voidEntityLibSection
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Literal
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Showable

voidEntityLibSection :: LibraryStuff
voidEntityLibSection =
    headingBDS
        "Void"
        ""
        [ typeBDS "Void" "A type with no values." (MkSomeGroundType voidGroundType) []
        , literalSubtypeRelationEntry @Void
        , showableSubtypeRelationEntry @Void "" never
        , namespaceBDS "Void"
            $ semigroupEntries @Void
            <> eqEntries @Void
            <> [ addNameInRootBDS $ valBDS "absurd" "" (absurd :: Void -> BottomType)
               ]
        ]
