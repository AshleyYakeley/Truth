module Pinafore.Documentation
    ( DefDoc(..)
    , DocTree(..)
    , runDocTree
    , libraryDoc
    , operatorFixity
    , Fixity(..)
    , FixAssoc(..)
    , Name(..)
    , nameIsInfix
    ) where

import Pinafore.Language.DocTree
import Pinafore.Language.Grammar
import Pinafore.Language.Library
import Pinafore.Language.Name
import Shapes

nameIsInfix :: Name -> Bool
nameIsInfix n =
    case unpack n of
        (c:_)
            | isAlpha c -> False
        "[]" -> False
        _ -> True
