module Pinafore.Edit
    ( Predicate(..)
    , Point(..)
    , PinaforeRead(..)
    , PinaforeEdit(..)
    ) where

import Data.Aeson (FromJSON)
import Data.Serialize as Serialize (Serialize(..))
import Data.UUID hiding (fromString, fromText, toText)
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

data PinaforeRead t where
    PinaforeReadGetValue :: Predicate -> Point -> PinaforeRead (Maybe Point)
    PinaforeReadLookupValue :: Predicate -> Point -> PinaforeRead (FiniteSet Point)
    PinaforeReadGetLiteral :: Point -> PinaforeRead (Maybe Text)
    PinaforeReadLookupLiteral :: Text -> PinaforeRead (FiniteSet Point)

instance Show (PinaforeRead t) where
    show (PinaforeReadGetValue p s) = "get " ++ show p ++ " of " ++ show s
    show (PinaforeReadLookupValue p v) = "lookup " ++ show p ++ " for " ++ show v
    show (PinaforeReadGetLiteral v) = "get literal of " ++ show v
    show (PinaforeReadLookupLiteral l) = "lookup literal for " ++ show l

data PinaforeEdit where
    PinaforeEditSetValue :: Predicate -> Point -> Maybe Point -> PinaforeEdit -- pred subj mval
    PinaforeEditSetLiteral :: Point -> Maybe Text -> PinaforeEdit

instance Show PinaforeEdit where
    show (PinaforeEditSetValue p s mv) = "set " ++ show p ++ " of " ++ show s ++ " to " ++ show mv
    show (PinaforeEditSetLiteral v ml) = "set literal of " ++ show v ++ " to " ++ show ml

instance SubjectReader PinaforeRead where
    type ReaderSubject PinaforeRead = ([(Predicate, Point, Point)], [(Point, Text)])
    subjectToRead (triples, _) (PinaforeReadGetValue rp rs) =
        listToMaybe $ [v | (p, s, v) <- triples, p == rp && s == rs]
    subjectToRead (triples, _) (PinaforeReadLookupValue rp rv) =
        MkFiniteSet [s | (p, s, v) <- triples, p == rp, v == rv]
    subjectToRead (_, literals) (PinaforeReadGetLiteral rv) = listToMaybe [l | (v, l) <- literals, v == rv]
    subjectToRead (_, literals) (PinaforeReadLookupLiteral rl) = MkFiniteSet [v | (v, l) <- literals, l == rl]

instance Floating PinaforeEdit PinaforeEdit

instance Edit PinaforeEdit where
    type EditReader PinaforeEdit = PinaforeRead
    applyEdit (PinaforeEditSetValue p s mv) _ (PinaforeReadGetValue p' s')
        | p == p' && s == s' = return mv
    applyEdit (PinaforeEditSetValue p s mv) mr (PinaforeReadLookupValue p' v')
        | p == p' = do
            fs <- mr $ PinaforeReadLookupValue p' v'
            return $
                case mv of
                    Just v
                        | v == v' -> insertSet s fs
                    _ -> deleteSet s fs
    applyEdit (PinaforeEditSetLiteral v ml) _mr (PinaforeReadGetLiteral v')
        | v == v' = return ml
    applyEdit (PinaforeEditSetLiteral v ml) mr (PinaforeReadLookupLiteral l') = do
        fv <- mr $ PinaforeReadLookupLiteral l'
        return $
            case ml of
                Just l
                    | l == l' -> insertSet v fv
                _ -> deleteSet v fv
    applyEdit _ mr rt = mr rt

instance InvertibleEdit PinaforeEdit where
    invertEdit (PinaforeEditSetValue p s _) mr = do
        mv <- mr $ PinaforeReadGetValue p s
        return [PinaforeEditSetValue p s mv]
    invertEdit (PinaforeEditSetLiteral v _) mr = do
        ml <- mr $ PinaforeReadGetLiteral v
        return [PinaforeEditSetLiteral v ml]
