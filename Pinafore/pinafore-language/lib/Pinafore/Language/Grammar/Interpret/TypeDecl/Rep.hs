module Pinafore.Language.Grammar.Interpret.TypeDecl.Rep
    ( makeDataTypeBox
    ) where

import qualified Data.List as List
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret.Type
import Pinafore.Language.Grammar.Interpret.TypeDecl.Constructor
import Pinafore.Language.Grammar.Interpret.TypeDecl.Mapping
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes
import Shapes.Unsafe (unsafeGetRefl)
