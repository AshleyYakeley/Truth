module Pinafore.Storage.Database where

import Pinafore.Base
import Pinafore.Storage.Table
import Shapes

data TripleTable t where
    TriplePredicate :: TripleTable Predicate
    TripleSubject :: TripleTable Entity
    TripleValue :: TripleTable Entity

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

data RefCountTable t where
    RefCountKey :: RefCountTable Entity
    RefCountValue :: RefCountTable RefCount

instance Show (RefCountTable t) where
    show RefCountKey = "key"
    show RefCountValue = "value"

instance AllWitnessConstraint Show RefCountTable where
    allWitnessConstraint = Dict

instance WitnessConstraint Show RefCountTable where
    witnessConstraint RefCountKey = Dict
    witnessConstraint RefCountValue = Dict

instance FiniteWitness RefCountTable where
    assembleWitnessF getw =
        (\k v ->
             MkAllF $ \case
                 RefCountKey -> k
                 RefCountValue -> v) <$>
        getw RefCountKey <*>
        getw RefCountValue

data LiteralTable t where
    LiteralKey :: LiteralTable Entity
    LiteralValue :: LiteralTable Literal

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
    PinaforeProperty :: PinaforeSchema TripleTable
    PinaforeRefCount :: PinaforeSchema RefCountTable
    PinaforeFact :: PinaforeSchema TripleTable
    PinaforeLiteral :: PinaforeSchema LiteralTable

instance Show (PinaforeSchema colsel) where
    show PinaforeProperty = "property"
    show PinaforeRefCount = "refcount"
    show PinaforeFact = "fact"
    show PinaforeLiteral = "literal"

instance AllWitnessConstraint Show PinaforeSchema where
    allWitnessConstraint = Dict

instance TestEquality PinaforeSchema where
    testEquality PinaforeProperty PinaforeProperty = Just Refl
    testEquality PinaforeRefCount PinaforeRefCount = Just Refl
    testEquality PinaforeFact PinaforeFact = Just Refl
    testEquality PinaforeLiteral PinaforeLiteral = Just Refl
    testEquality _ _ = Nothing

instance FiniteWitness PinaforeSchema where
    assembleWitnessF getTable =
        (\ft fr ff fl ->
             MkAllF $ \case
                 PinaforeProperty -> ft
                 PinaforeRefCount -> fr
                 PinaforeFact -> ff
                 PinaforeLiteral -> fl) <$>
        getTable PinaforeProperty <*>
        getTable PinaforeRefCount <*>
        getTable PinaforeFact <*>
        getTable PinaforeLiteral

instance WitnessConstraint FiniteWitness PinaforeSchema where
    witnessConstraint PinaforeProperty = Dict
    witnessConstraint PinaforeRefCount = Dict
    witnessConstraint PinaforeFact = Dict
    witnessConstraint PinaforeLiteral = Dict

instance WitnessConstraint (AllWitnessConstraint Show) PinaforeSchema where
    witnessConstraint PinaforeProperty = Dict
    witnessConstraint PinaforeRefCount = Dict
    witnessConstraint PinaforeFact = Dict
    witnessConstraint PinaforeLiteral = Dict

instance WitnessConstraint (WitnessConstraint Show) PinaforeSchema where
    witnessConstraint PinaforeProperty = Dict
    witnessConstraint PinaforeRefCount = Dict
    witnessConstraint PinaforeFact = Dict
    witnessConstraint PinaforeLiteral = Dict
