module Pinafore.Entity
    ( Predicate(..)
    , PinaforeEntityRead(..)
    , PinaforeEntityEdit(..)
    ) where

import Data.Aeson (FromJSON)
import Data.UUID hiding (fromString)
import Pinafore.Know
import Pinafore.Literal
import Pinafore.Point
import Shapes
import Truth.Core

newtype Predicate =
    MkPredicate UUID
    deriving (Eq, FromJSON)

instance Show Predicate where
    show (MkPredicate uuid) = '%' : show uuid

-- | Some of these reads may add to the database, but will always give consistent results between changes.
data PinaforeEntityRead t where
    PinaforeEntityReadGetPredicate :: Predicate -> Point -> PinaforeEntityRead Point
    PinaforeEntityReadLookupPredicate :: Predicate -> Point -> PinaforeEntityRead (FiniteSet Point)
    PinaforeEntityReadToLiteral :: AsLiteral t => Point -> PinaforeEntityRead (Know t)
    PinaforeEntityReadFromLiteral :: AsLiteral t => t -> PinaforeEntityRead Point
    PinaforeEntityReadUnit :: PinaforeEntityRead Point
    PinaforeEntityReadToPair :: Point -> PinaforeEntityRead (Know (Point, Point))
    PinaforeEntityReadFromPair :: (Point, Point) -> PinaforeEntityRead Point
    PinaforeEntityReadToEither :: Point -> PinaforeEntityRead (Know (Either Point Point))
    PinaforeEntityReadFromEither :: Either Point Point -> PinaforeEntityRead Point

instance Show (PinaforeEntityRead t) where
    show (PinaforeEntityReadGetPredicate p s) = "get " ++ show p ++ " of " ++ show s
    show (PinaforeEntityReadLookupPredicate p v) = "lookup " ++ show p ++ " for " ++ show v
    show (PinaforeEntityReadToLiteral v) = "to literal " ++ show v
    show (PinaforeEntityReadFromLiteral lit) = "from literal " ++ show (toLiteral lit)
    show PinaforeEntityReadUnit = "unit"
    show (PinaforeEntityReadToPair p) = "to pair " ++ show p
    show (PinaforeEntityReadFromPair ab) = "from pair " ++ show ab
    show (PinaforeEntityReadToEither p) = "to either " ++ show p
    show (PinaforeEntityReadFromEither eab) = "from either " ++ show eab

instance AllWitnessConstraint Show PinaforeEntityRead where
    allWitnessConstraint = Dict

instance WitnessConstraint Show PinaforeEntityRead where
    witnessConstraint (PinaforeEntityReadGetPredicate _ _) = Dict
    witnessConstraint (PinaforeEntityReadLookupPredicate _ _) = Dict
    witnessConstraint (PinaforeEntityReadToLiteral _) = Dict
    witnessConstraint (PinaforeEntityReadFromLiteral _) = Dict
    witnessConstraint PinaforeEntityReadUnit = Dict
    witnessConstraint (PinaforeEntityReadToPair _) = Dict
    witnessConstraint (PinaforeEntityReadFromPair _) = Dict
    witnessConstraint (PinaforeEntityReadToEither _) = Dict
    witnessConstraint (PinaforeEntityReadFromEither _) = Dict

data PinaforeEntityEdit where
    PinaforeEntityEditSetPredicate :: Predicate -> Point -> Know Point -> PinaforeEntityEdit -- pred subj kval

type instance EditReader PinaforeEntityEdit = PinaforeEntityRead

instance Show PinaforeEntityEdit where
    show (PinaforeEntityEditSetPredicate p s kv) = "set " ++ show p ++ " of " ++ show s ++ " to " ++ show kv
