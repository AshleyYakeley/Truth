module Pinafore.Language.Name.ToText where

import Language.Expression.Common
import Shapes

class ToText t where
    toText :: t -> Text

instance ToText Text where
    toText t = t

instance ToText (SymbolType name) where
    toText n = pack $ uVarName n
