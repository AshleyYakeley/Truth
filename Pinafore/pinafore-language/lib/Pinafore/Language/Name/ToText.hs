module Pinafore.Language.Name.ToText where

import Language.Expression.Common
import Shapes

class ToText t where
    toText :: t -> Text

instance ToText Text where
    toText t = t

instance {-# OVERLAPPABLE #-} ToText t => ToText [t] where
    toText ft = mconcat $ fmap toText ft

instance ToText (SymbolType name) where
    toText n = pack $ uVarName n
