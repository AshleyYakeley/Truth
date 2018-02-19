module Pinafore.Database where

import Pinafore.Edit
import Shapes

data TripleTable t where
    TriplePredicate :: TripleTable Predicate
    TripleSubject :: TripleTable Point
    TripleValue :: TripleTable Point

instance Show (TripleTable t) where
    show TriplePredicate = "predicate"
    show TripleSubject = "subject"
    show TripleValue = "value"

instance AllWitnessConstraint Show TripleTable where
    allWitnessConstraint = Dict

instance WitnessConstraint Show TripleTable where
    witnessConstraint TriplePredicate = Dict
    witnessConstraint TripleSubject = Dict
    witnessConstraint TripleValue = Dict

instance FiniteWitness TripleTable where
    assembleWitnessF getw =
        (\p s v ->
             MkAllF $ \case
                 TriplePredicate -> p
                 TripleSubject -> s
                 TripleValue -> v) <$>
        getw TriplePredicate <*>
        getw TripleSubject <*>
        getw TripleValue

data LiteralTable t where
    LiteralKey :: LiteralTable Point
    LiteralValue :: LiteralTable Text

instance Show (LiteralTable t) where
    show LiteralKey = "key"
    show LiteralValue = "value"

instance AllWitnessConstraint Show LiteralTable where
    allWitnessConstraint = Dict

instance WitnessConstraint Show LiteralTable where
    witnessConstraint LiteralKey = Dict
    witnessConstraint LiteralValue = Dict

instance FiniteWitness LiteralTable where
    assembleWitnessF getw =
        (\k v ->
             MkAllF $ \case
                 LiteralKey -> k
                 LiteralValue -> v) <$>
        getw LiteralKey <*>
        getw LiteralValue

data PinaforeSchema colsel where
    PinaforeTriple :: PinaforeSchema TripleTable
    PinaforeLiteral :: PinaforeSchema LiteralTable

instance Show (PinaforeSchema colsel) where
    show PinaforeTriple = "triple"
    show PinaforeLiteral = "literal"

instance AllWitnessConstraint Show PinaforeSchema where
    allWitnessConstraint = Dict

instance TestEquality PinaforeSchema where
    testEquality PinaforeTriple PinaforeTriple = Just Refl
    testEquality PinaforeLiteral PinaforeLiteral = Just Refl
    testEquality _ _ = Nothing

instance FiniteWitness PinaforeSchema where
    assembleWitnessF getTable =
        (\ft fl ->
             MkAllF $ \case
                 PinaforeTriple -> ft
                 PinaforeLiteral -> fl) <$>
        getTable PinaforeTriple <*>
        getTable PinaforeLiteral
