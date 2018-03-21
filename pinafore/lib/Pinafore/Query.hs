module Pinafore.Query
    ( QType(..)
    , QValue
    , HasQTypeDescription(..)
    , ToQValue(..)
    , QBindings
    , qdisplay
    , parseValue
    , predefinedDoc
    ) where

import Pinafore.Query.Convert
import Pinafore.Query.Expression
import Pinafore.Query.Predefined
import Pinafore.Query.Read
import Pinafore.Query.Value
import Pinafore.Table
import Shapes

parseValue ::
       forall baseedit t. (HasPinaforeTableEdit baseedit, FromQValue baseedit t)
    => String
    -> Text
    -> Result Text t
parseValue name text = do
    expr <- parseExpression @baseedit name text
    val <- qeval $ qlets predefinedBindings expr
    fromQValue val
