module Pinafore.Documentation
    ( module Pinafore.Text
    , DefDoc(..)
    , DocItem(..)
    , operatorFixity
    , typeOperatorFixity
    , Fixity(..)
    , FixAssoc(..)
    , Name(..)
    , ToText(..)
    , nameIsInfix
    , allKeywords
    , allOperatorNames
    ) where

import Pinafore.Language.DefDoc
import Pinafore.Language.Grammar
import Pinafore.Language.Library
import Pinafore.Language.Name
import Pinafore.Text
