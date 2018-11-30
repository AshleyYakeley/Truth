module Pinafore.Base.Edit
    ( Predicate(..)
    , PinaforeEntityRead(..)
    , PinaforeEntityEdit(..)
    ) where

import Data.Aeson (FromJSON)
import Pinafore.Base.Entity
import Pinafore.Base.Know
import Pinafore.Base.Literal
import Shapes
import Truth.Core

newtype Predicate =
    MkPredicate Anchor
    deriving (Eq, FromJSON, Show)

-- | Some of these reads may add to the database, but will always give consistent results between changes.
data PinaforeEntityRead t where
    PinaforeEntityReadGetPredicate :: Predicate -> Entity -> PinaforeEntityRead (Know Entity)
    PinaforeEntityReadGetProperty :: Predicate -> Entity -> PinaforeEntityRead Entity
    PinaforeEntityReadLookupPredicate :: Predicate -> Entity -> PinaforeEntityRead (FiniteSet Entity)
    PinaforeEntityReadToLiteral :: Entity -> PinaforeEntityRead (Know Literal)

instance Show (PinaforeEntityRead t) where
    show (PinaforeEntityReadGetPredicate p s) = "get " ++ show p ++ " of " ++ show s
    show (PinaforeEntityReadGetProperty p s) = "get prop " ++ show p ++ " of " ++ show s
    show (PinaforeEntityReadLookupPredicate p v) = "lookup " ++ show p ++ " for " ++ show v
    show (PinaforeEntityReadToLiteral v) = "to literal " ++ show v

instance AllWitnessConstraint Show PinaforeEntityRead where
    allWitnessConstraint = Dict

instance WitnessConstraint Show PinaforeEntityRead where
    witnessConstraint (PinaforeEntityReadGetPredicate _ _) = Dict
    witnessConstraint (PinaforeEntityReadGetProperty _ _) = Dict
    witnessConstraint (PinaforeEntityReadLookupPredicate _ _) = Dict
    witnessConstraint (PinaforeEntityReadToLiteral _) = Dict

data PinaforeEntityEdit where
    PinaforeEntityEditSetPredicate :: Predicate -> Entity -> Know Entity -> PinaforeEntityEdit -- pred subj kval
    PinaforeEntityEditSetLiteral :: Entity -> Know Literal -> PinaforeEntityEdit

type instance EditReader PinaforeEntityEdit = PinaforeEntityRead

instance Show PinaforeEntityEdit where
    show (PinaforeEntityEditSetPredicate p s kv) = "set " ++ show p ++ " of " ++ show s ++ " to " ++ show kv
    show (PinaforeEntityEditSetLiteral p kl) = "set " ++ show p ++ " to " ++ show kl
