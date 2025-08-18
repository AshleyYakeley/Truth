module Pinafore.Language.If
    ( qifthenelse
    )
where

import Import
import Pinafore.Language.Convert.Var

qifthenelse :: Bool -> A -> A -> A
qifthenelse True v _ = v
qifthenelse False _ v = v
