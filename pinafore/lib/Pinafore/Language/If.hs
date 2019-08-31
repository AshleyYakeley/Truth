module Pinafore.Language.If
    ( qifthenelse
    , qbind
    , qbind_
    ) where

import Data.Shim
import Pinafore.Base
import Pinafore.Language.TypeSystem
import Shapes

type A = UVar "a"

type B = UVar "b"

qifthenelse :: Bool -> A -> A -> A
qifthenelse True v _ = v
qifthenelse False _ v = v

qbind :: PinaforeAction baseupdate A -> (A -> PinaforeAction baseupdate B) -> PinaforeAction baseupdate B
qbind = (>>=)

qbind_ :: PinaforeAction baseupdate TopType -> PinaforeAction baseupdate A -> PinaforeAction baseupdate A
qbind_ = (>>)
