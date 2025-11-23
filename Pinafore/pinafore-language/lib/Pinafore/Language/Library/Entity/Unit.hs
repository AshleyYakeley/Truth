module Pinafore.Language.Library.Entity.Unit
    ( unitEntityLibSection
    )
where

import Import
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule

unitEntityLibSection :: LibraryStuff
unitEntityLibSection =
    headingBDS
        "Unit"
        ""
        [ typeBDS_ @_ @()
            "Unit"
            "A type with one value, `()`."
            [ addNameInRootBDS
                $ valPatBDS "[]" "Empty value" ()
                $ PureFunction
                $ pure
                $ \() -> ()
            ]
        , namespaceBDS "Unit" $ monoidEntries @() <> eqEntries @()
        ]
