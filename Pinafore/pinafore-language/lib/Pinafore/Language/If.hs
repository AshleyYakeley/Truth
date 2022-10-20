module Pinafore.Language.If
    ( qifthenelse
    , qbind
    , qbind_
    ) where

import Pinafore.Base
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes

qifthenelse :: Bool -> A -> A -> A
qifthenelse True v _ = v
qifthenelse False _ v = v

qbind :: Action A -> (A -> Action B) -> Action B
qbind = (>>=)

qbind_ :: Action TopType -> Action A -> Action A
qbind_ = (>>)
