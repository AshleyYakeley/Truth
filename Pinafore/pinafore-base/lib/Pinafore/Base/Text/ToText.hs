module Pinafore.Base.Text.ToText where

import Shapes

-- | for text-like types only
class ToText t where
    toText :: t -> Text

instance ToText Text where
    toText t = t

instance ToText String where
    toText = pack

instance {-# OVERLAPPABLE #-} ToText t => ToText [t] where
    toText ft = concatmap toText ft
