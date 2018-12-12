module Pinafore.Language.If
    ( qifthenelse
    ) where

import Pinafore.Language.Type
import Shapes

qifthenelse :: Bool -> UVar "a" -> UVar "a" -> UVar "a"
qifthenelse True v _ = v
qifthenelse False _ v = v
