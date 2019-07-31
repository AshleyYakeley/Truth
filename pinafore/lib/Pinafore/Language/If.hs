module Pinafore.Language.If
    ( qifthenelse
    , qbind
    , qbind_
    ) where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.Type
import Shapes

type A = UVar "a"

type B = UVar "b"

qifthenelse :: Bool -> A -> A -> A
qifthenelse True v _ = v
qifthenelse False _ v = v

qbind :: PinaforeAction baseedit A -> (A -> PinaforeAction baseedit B) -> PinaforeAction baseedit B
qbind = (>>=)

qbind_ :: PinaforeAction baseedit TopType -> PinaforeAction baseedit A -> PinaforeAction baseedit A
qbind_ = (>>)
