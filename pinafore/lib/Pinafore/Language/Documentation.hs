module Pinafore.Language.Documentation
    ( DefDoc(..)
    , DocTree
    , runDocTree
    , predefinedDoc
    , operatorFixity
    , Fixity(..)
    , FixAssoc(..)
    , Name(..)
    , nameIsInfix
    ) where

import Pinafore.Language.DocTree
import Pinafore.Language.Name
import Pinafore.Language.Predefined
import Pinafore.Language.Read.Infix
import Shapes

nameIsInfix :: Name -> Bool
nameIsInfix n =
    case unpack n of
        (c:_)
            | isAlpha c -> False
        "[]" -> False
        _ -> True
