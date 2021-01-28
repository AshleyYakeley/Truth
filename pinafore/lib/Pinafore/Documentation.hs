module Pinafore.Documentation
    ( DefDoc(..)
    , DocTree(..)
    , runDocTree
    , libraryDoc
    , operatorFixity
    , Fixity(..)
    , FixAssoc(..)
    , Name(..)
    , allOperatorNames
    ) where

import Pinafore.Language.DocTree
import Pinafore.Language.Grammar
import Pinafore.Language.Library
import Pinafore.Language.Name
