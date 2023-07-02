module Pinafore.Language.If
    ( qifthenelse
    ) where

import Pinafore.Language.Var
import Shapes

qifthenelse :: Bool -> A -> A -> A
qifthenelse True v _ = v
qifthenelse False _ v = v
