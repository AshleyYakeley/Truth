module Pinafore.Language.Library.Product
    ( productLibSection
    )
where

import Import
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type
import Pinafore.Language.Var

productLibSection :: LibraryStuff
productLibSection =
    headingBDS
        "Type Product"
        ""
        [ typeBDS "*:" "" (MkSomeGroundType pairGroundType) []
        , hasSubtypeRelationBDS @(Entity, Entity) @Entity Verify ""
            $ functionToShim "pairEntityConvert" pairEntityConvert
        , hasSubtypeRelationBDS @(Showable, Showable) @Showable Verify "" $ functionToShim "show" textShowable
        , namespaceBDS
            "Product"
            [ addNameInRootBDS $ valBDS "fst" "Get the first member of a pair." $ fst @A @B
            , addNameInRootBDS $ valBDS "snd" "Get the second member of a pair." $ snd @A @B
            , valBDS "to" "Construct a pair." $ (,) @A @B
            , addNameInRootBDS $ valBDS "both" "Construct a pair." $ \(a :: A) -> (a, a)
            ]
        ]
