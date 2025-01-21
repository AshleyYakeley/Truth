module Pinafore.Syntax.Name.ImplicitName where

import Pinafore.Base
import Shapes

import Pinafore.Syntax.Name.Name

newtype ImplicitName
    = MkImplicitName Name
    deriving newtype (Eq, Ord)

instance ShowText ImplicitName where
    showText (MkImplicitName n) = "?" <> showText n

instance Show ImplicitName where
    show n = unpack $ showText n
