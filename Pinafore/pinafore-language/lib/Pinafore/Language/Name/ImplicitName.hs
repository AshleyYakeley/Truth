module Pinafore.Language.Name.ImplicitName where

import Pinafore.Language.Name.Name
import Pinafore.Text
import Shapes

newtype ImplicitName =
    MkImplicitName Name
    deriving (Eq, Ord)

instance ShowText ImplicitName where
    showText (MkImplicitName n) = "?" <> showText n

instance Show ImplicitName where
    show n = unpack $ showText n
