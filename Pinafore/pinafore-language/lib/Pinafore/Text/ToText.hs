module Pinafore.Text.ToText where

import Language.Expression.Common
import Shapes

-- | for text-like types only
class ToText t where
    toText :: t -> Text

instance ToText Text where
    toText t = t

instance ToText String where
    toText = pack

instance {-# OVERLAPPABLE #-} ToText t => ToText [t] where
    toText ft = mconcat $ fmap toText ft

class ShowText t where
    showText :: t -> Text
    default showText :: Show t => t -> Text
    showText = pack . show

instance {-# OVERLAPPABLE #-} Show t => ShowText t

instance ShowText (SymbolType name) where
    showText n = pack $ uVarName n
