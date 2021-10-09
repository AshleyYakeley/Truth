module Pinafore.Language.VarID
    ( VarID(..)
    , VarIDState
    , firstVarIDState
    , nextVarIDState
    , mkVarID
    , mkBadVarID
    ) where

import Pinafore.Language.Name
import Shapes
import Text.Parsec (SourcePos)

newtype VarIDState =
    MkVarIDState Int

firstVarIDState :: VarIDState
firstVarIDState = MkVarIDState 0

nextVarIDState :: VarIDState -> VarIDState
nextVarIDState (MkVarIDState i) = MkVarIDState $ succ i

data VarID
    = GoodVarID Int
                Name
    | BadVarID SourcePos
               ReferenceName

instance Eq VarID where
    GoodVarID s1 _ == GoodVarID s2 _ = s1 == s2
    BadVarID _ n1 == BadVarID _ n2 = n1 == n2
    _ == _ = False

instance Ord VarID where
    compare (GoodVarID s1 _) (GoodVarID s2 _) = compare s1 s2
    compare (BadVarID _ n1) (BadVarID _ n2) = compare n1 n2
    compare (BadVarID _ _) (GoodVarID _ _) = LT
    compare (GoodVarID _ _) (BadVarID _ _) = GT

instance Show VarID where
    show (GoodVarID _ n) = show n
    show (BadVarID _ n) = show n

mkVarID :: VarIDState -> Name -> VarID
mkVarID (MkVarIDState s) = GoodVarID s

-- We could just throw an exception here, but this way we get to see the type of the missing variable.
mkBadVarID :: SourcePos -> ReferenceName -> VarID
mkBadVarID = BadVarID
