module Pinafore.Syntax.Name.ImplicitName where

import Pinafore.Base
import Pinafore.Syntax.Name.Name
import Shapes

newtype ImplicitName =
    MkImplicitName Name
    deriving (Eq, Ord)

instance ShowText ImplicitName where
    showText (MkImplicitName n) = "?" <> showText n

instance Show ImplicitName where
    show n = unpack $ showText n
