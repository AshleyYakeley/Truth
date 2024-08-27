module Pinafore.Language.Library.Entity.Boolean
    ( booleanEntityLibSection
    ) where

import Import
import Pinafore.Language.Convert.Types
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Literal
import Pinafore.Language.Library.Entity.Showable
import Pinafore.Language.Library.LibraryModule

booleanEntityLibSection :: LibraryStuff
booleanEntityLibSection =
    headingBDS
        "Boolean"
        ""
        [ typeBDS "Boolean" "" (MkSomeGroundType booleanGroundType) $
          fmap
              addNameInRootBDS
              [ valPatBDS "True" "Boolean TRUE." True $
                ImpureFunction $
                pure $ \v ->
                    if v
                        then Just ()
                        else Nothing
              , valPatBDS "False" "Boolean FALSE." False $
                ImpureFunction $
                pure $ \v ->
                    if v
                        then Nothing
                        else Just ()
              ]
        , literalSubtypeRelationEntry @Bool
        , showableSubtypeRelationEntry @Bool
        , namespaceBDS "Boolean" $
          eqEntries @Bool <>
          [ addNameInRootBDS $ valBDS "&&" "Boolean AND." (&&)
          , addNameInRootBDS $ valBDS "||" "Boolean OR." (||)
          , addNameInRootBDS $ valBDS "not" "Boolean NOT." not
          ]
        ]
