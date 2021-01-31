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

qbind :: PinaforeAction A -> (A -> PinaforeAction B) -> PinaforeAction B
qbind = (>>=)

qbind_ :: PinaforeAction TopType -> PinaforeAction A -> PinaforeAction A
qbind_ = (>>)
