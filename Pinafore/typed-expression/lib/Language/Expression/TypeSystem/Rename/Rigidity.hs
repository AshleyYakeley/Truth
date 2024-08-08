module Language.Expression.TypeSystem.Rename.Rigidity where

import Shapes

data NameRigidity
    = FreeName
    | RigidName

instance Show NameRigidity where
    show FreeName = "free"
    show RigidName = "rigid"
