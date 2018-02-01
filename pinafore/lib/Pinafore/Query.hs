module Pinafore.Query
    ( QType(..)
    , QValue
    , FromQValue(..)
    , QBindings
    , qdisplay
    , parseValue
    , predefinedDoc
    ) where

import Pinafore.Query.Expression
import Pinafore.Query.Predefined
import Pinafore.Query.Read
import Pinafore.Query.Value
import Shapes

parseValue :: FromQValue t => String -> String -> Result String t
parseValue name text = do
    expr <- parseExpression name text
    val <- qeval $ qlets predefinedBindings expr
    fromQValue val
