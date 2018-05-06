module Pinafore.Point
    ( Predicate(..)
    , Point(..)
    , newPoint
    , PinaforePointRead(..)
    , PinaforePointEdit(..)
    ) where

import Data.Aeson (FromJSON)
import Data.Serialize as Serialize (Serialize(..))
import Data.UUID hiding (fromString)
import Pinafore.Literal
import Shapes
import Truth.Core

newtype Predicate =
    MkPredicate UUID
    deriving (Eq, FromJSON)

instance Show Predicate where
    show (MkPredicate uuid) = '%' : show uuid

newtype Point =
    MkPoint UUID
    deriving (Eq, Random, FromJSON)

instance Show Point where
    show (MkPoint uuid) = '!' : show uuid

instance Serialize Point where
    put (MkPoint uuid) = Serialize.put (toByteString uuid)
    get = do
        bs <- Serialize.get
        case fromByteString bs of
            Just uuid -> return $ MkPoint uuid
            Nothing -> fail "deserialize bad UUID"

newPoint :: MonadIO m => m Point
newPoint = liftIO randomIO

-- | Some of these reads may add to the database, but will always give consistent results between changes.
data PinaforePointRead t where
    PinaforePointReadGetPredicate :: Predicate -> Point -> PinaforePointRead Point
    PinaforePointReadLookupPredicate :: Predicate -> Point -> PinaforePointRead (FiniteSet Point)
    PinaforePointReadToLiteral :: AsLiteral t => Point -> PinaforePointRead (Maybe t)
    PinaforePointReadFromLiteral :: AsLiteral t => t -> PinaforePointRead Point
    PinaforePointReadUnit :: PinaforePointRead Point
    PinaforePointReadToPair :: Point -> PinaforePointRead (Maybe (Point, Point))
    PinaforePointReadFromPair :: (Point, Point) -> PinaforePointRead Point
    PinaforePointReadToEither :: Point -> PinaforePointRead (Maybe (Either Point Point))
    PinaforePointReadFromEither :: Either Point Point -> PinaforePointRead Point

instance Show (PinaforePointRead t) where
    show (PinaforePointReadGetPredicate p s) = "get " ++ show p ++ " of " ++ show s
    show (PinaforePointReadLookupPredicate p v) = "lookup " ++ show p ++ " for " ++ show v
    show (PinaforePointReadToLiteral v) = "to literal " ++ show v
    show (PinaforePointReadFromLiteral lit) = "from literal " ++ show (toLiteral lit)
    show PinaforePointReadUnit = "unit"
    show (PinaforePointReadToPair p) = "to pair " ++ show p
    show (PinaforePointReadFromPair ab) = "from pair " ++ show ab
    show (PinaforePointReadToEither p) = "to either " ++ show p
    show (PinaforePointReadFromEither eab) = "from either " ++ show eab

instance AllWitnessConstraint Show PinaforePointRead where
    allWitnessConstraint = Dict

instance WitnessConstraint Show PinaforePointRead where
    witnessConstraint (PinaforePointReadGetPredicate _ _) = Dict
    witnessConstraint (PinaforePointReadLookupPredicate _ _) = Dict
    witnessConstraint (PinaforePointReadToLiteral _) = Dict
    witnessConstraint (PinaforePointReadFromLiteral _) = Dict
    witnessConstraint PinaforePointReadUnit = Dict
    witnessConstraint (PinaforePointReadToPair _) = Dict
    witnessConstraint (PinaforePointReadFromPair _) = Dict
    witnessConstraint (PinaforePointReadToEither _) = Dict
    witnessConstraint (PinaforePointReadFromEither _) = Dict

data PinaforePointEdit where
    PinaforePointEditSetPredicate :: Predicate -> Point -> Point -> PinaforePointEdit -- pred subj val

type instance EditReader PinaforePointEdit = PinaforePointRead

instance Show PinaforePointEdit where
    show (PinaforePointEditSetPredicate p s v) = "set " ++ show p ++ " of " ++ show s ++ " to " ++ show v
