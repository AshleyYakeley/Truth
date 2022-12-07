module Pinafore.Language.VarID
    ( VarID(..)
    , varIdNameRef
    , VarIDState
    , firstVarIDState
    , nextVarIDState
    , mkVarID
    , mkUniqueVarID
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
                FullName
    | BadVarID SourcePos
               FullNameRef

instance Eq VarID where
    GoodVarID s1 _ == GoodVarID s2 _ = s1 == s2
    BadVarID _ n1 == BadVarID _ n2 = n1 == n2
    _ == _ = False

instance Ord VarID where
    compare (GoodVarID s1 _) (GoodVarID s2 _) = compare s1 s2
    compare (BadVarID _ n1) (BadVarID _ n2) = compare n1 n2
    compare (BadVarID _ _) (GoodVarID _ _) = LT
    compare (GoodVarID _ _) (BadVarID _ _) = GT

instance ExprShow VarID where
    exprShowPrec (GoodVarID _ n) = exprShowPrec n
    exprShowPrec (BadVarID _ n) = exprShowPrec n

instance Show VarID where
    show (GoodVarID _ n) = show n
    show (BadVarID _ n) = show n

varIdNameRef :: VarID -> FullNameRef
varIdNameRef (GoodVarID _ n) = fullNameRef n
varIdNameRef (BadVarID _ n) = n

mkVarID :: VarIDState -> FullName -> VarID
mkVarID (MkVarIDState s) = GoodVarID s

mkUniqueVarID :: VarIDState -> (VarID, FullName)
mkUniqueVarID (MkVarIDState s) = let
    name :: FullName
    name = fromString $ "%var-" <> show s
    in (GoodVarID s name, name)

-- We could just throw an exception here, but this way we get to see the type of the missing variable.
mkBadVarID :: SourcePos -> FullNameRef -> VarID
mkBadVarID = BadVarID
