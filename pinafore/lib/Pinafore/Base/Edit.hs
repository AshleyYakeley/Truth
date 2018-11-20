module Pinafore.Base.Edit
    ( Predicate(..)
    , PinaforePointRead(..)
    , PinaforePointEdit(..)
    ) where

import Data.Aeson (FromJSON)
import Pinafore.Base.Know
import Pinafore.Base.Literal
import Pinafore.Base.Point
import Shapes
import Truth.Core

newtype Predicate =
    MkPredicate Anchor
    deriving (Eq, FromJSON, Show)

-- | Some of these reads may add to the database, but will always give consistent results between changes.
data PinaforePointRead t where
    PinaforePointReadGetPredicate :: Predicate -> Point -> PinaforePointRead Point
    PinaforePointReadLookupPredicate :: Predicate -> Point -> PinaforePointRead (FiniteSet Point)
    PinaforePointReadToLiteral :: Point -> PinaforePointRead (Know Literal)

instance Show (PinaforePointRead t) where
    show (PinaforePointReadGetPredicate p s) = "get " ++ show p ++ " of " ++ show s
    show (PinaforePointReadLookupPredicate p v) = "lookup " ++ show p ++ " for " ++ show v
    show (PinaforePointReadToLiteral v) = "to literal " ++ show v

instance AllWitnessConstraint Show PinaforePointRead where
    allWitnessConstraint = Dict

instance WitnessConstraint Show PinaforePointRead where
    witnessConstraint (PinaforePointReadGetPredicate _ _) = Dict
    witnessConstraint (PinaforePointReadLookupPredicate _ _) = Dict
    witnessConstraint (PinaforePointReadToLiteral _) = Dict

data PinaforePointEdit where
    PinaforePointEditSetPredicate :: Predicate -> Point -> Know Point -> PinaforePointEdit -- pred subj kval
    PinaforePointEditSetLiteral :: Point -> Know Literal -> PinaforePointEdit

type instance EditReader PinaforePointEdit = PinaforePointRead

instance Show PinaforePointEdit where
    show (PinaforePointEditSetPredicate p s kv) = "set " ++ show p ++ " of " ++ show s ++ " to " ++ show kv
    show (PinaforePointEditSetLiteral p kl) = "set " ++ show p ++ " to " ++ show kl
