module Pinafore.Base.File where

import Shapes

import Pinafore.Base.Entity

newtype FileEntity
    = MkFileEntity Entity
    deriving newtype Eq
