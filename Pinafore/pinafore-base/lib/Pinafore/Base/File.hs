module Pinafore.Base.File where

import Pinafore.Base.Entity
import Shapes

newtype FileEntity =
    MkFileEntity Entity
    deriving (Eq)
