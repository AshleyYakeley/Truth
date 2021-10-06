module Pinafore.Language.VarID
    ( VarID
    , VarIDState
    , firstVarIDState
    , nextVarIDState
    , mkVarID
    ) where

import Pinafore.Language.Name
import Shapes

newtype VarIDState =
    MkVarIDState Int

firstVarIDState :: VarIDState
firstVarIDState = MkVarIDState 0

nextVarIDState :: VarIDState -> VarIDState
nextVarIDState (MkVarIDState i) = MkVarIDState $ succ i

data VarID =
    MkVarID Int
            Name

instance Eq VarID where
    MkVarID s1 _ == MkVarID s2 _ = s1 == s2

instance Ord VarID where
    compare (MkVarID s1 _) (MkVarID s2 _) = compare s1 s2

instance Show VarID where
    show (MkVarID _ n) = show n

mkVarID :: VarIDState -> Name -> VarID
mkVarID (MkVarIDState s) = MkVarID s
