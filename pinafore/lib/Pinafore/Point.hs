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

data PinaforePointEdit where
    PinaforePointEditSetPredicate :: Predicate -> Point -> Point -> PinaforePointEdit -- pred subj val

type instance EditReader PinaforePointEdit = PinaforePointRead
