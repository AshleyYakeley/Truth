module Pinafore.Storage.Database where

import Pinafore.Base
import Shapes

import Pinafore.Storage.Table

data TripleTable t where
    TriplePredicate :: TripleTable Predicate
    TripleSubject :: TripleTable Entity
    TripleValue :: TripleTable Entity

instance Show (TripleTable t) where
    show TriplePredicate = "predicate"
    show TripleSubject = "subject"
    show TripleValue = "value"

instance AllConstraint Show TripleTable where
    allConstraint = Dict

instance WitnessConstraint Show TripleTable where
    witnessConstraint TriplePredicate = Dict
    witnessConstraint TripleSubject = Dict
    witnessConstraint TripleValue = Dict

instance FiniteWitness TripleTable where
    assembleAllFor getw =
        ( \p s v ->
            MkAllFor $ \case
                TriplePredicate -> p
                TripleSubject -> s
                TripleValue -> v
        )
            <$> getw TriplePredicate
            <*> getw TripleSubject
            <*> getw TripleValue

data RefCountTable t where
    RefCountKey :: RefCountTable Entity
    RefCountValue :: RefCountTable RefCount

instance Show (RefCountTable t) where
    show RefCountKey = "key"
    show RefCountValue = "value"

instance AllConstraint Show RefCountTable where
    allConstraint = Dict

instance WitnessConstraint Show RefCountTable where
    witnessConstraint RefCountKey = Dict
    witnessConstraint RefCountValue = Dict

instance FiniteWitness RefCountTable where
    assembleAllFor getw =
        ( \k v ->
            MkAllFor $ \case
                RefCountKey -> k
                RefCountValue -> v
        )
            <$> getw RefCountKey
            <*> getw RefCountValue

data LiteralTable t where
    LiteralKey :: LiteralTable Entity
    LiteralValue :: LiteralTable Literal

instance Show (LiteralTable t) where
    show LiteralKey = "key"
    show LiteralValue = "value"

instance AllConstraint Show LiteralTable where
    allConstraint = Dict

instance WitnessConstraint Show LiteralTable where
    witnessConstraint LiteralKey = Dict
    witnessConstraint LiteralValue = Dict

instance FiniteWitness LiteralTable where
    assembleAllFor getw =
        ( \k v ->
            MkAllFor $ \case
                LiteralKey -> k
                LiteralValue -> v
        )
            <$> getw LiteralKey
            <*> getw LiteralValue

data QSchema colsel where
    QSProperty :: QSchema TripleTable
    QSModelCount :: QSchema RefCountTable
    QSFact :: QSchema TripleTable
    QSLiteral :: QSchema LiteralTable

instance Show (QSchema colsel) where
    show QSProperty = "property"
    show QSModelCount = "refcount"
    show QSFact = "fact"
    show QSLiteral = "literal"

instance AllConstraint Show QSchema where
    allConstraint = Dict

instance TestEquality QSchema where
    testEquality QSProperty QSProperty = Just Refl
    testEquality QSModelCount QSModelCount = Just Refl
    testEquality QSFact QSFact = Just Refl
    testEquality QSLiteral QSLiteral = Just Refl
    testEquality _ _ = Nothing

instance FiniteWitness QSchema where
    assembleAllFor getTable =
        ( \ft fr ff fl ->
            MkAllFor $ \case
                QSProperty -> ft
                QSModelCount -> fr
                QSFact -> ff
                QSLiteral -> fl
        )
            <$> getTable QSProperty
            <*> getTable QSModelCount
            <*> getTable QSFact
            <*> getTable QSLiteral

instance WitnessConstraint FiniteWitness QSchema where
    witnessConstraint QSProperty = Dict
    witnessConstraint QSModelCount = Dict
    witnessConstraint QSFact = Dict
    witnessConstraint QSLiteral = Dict

instance WitnessConstraint (AllConstraint Show) QSchema where
    witnessConstraint QSProperty = Dict
    witnessConstraint QSModelCount = Dict
    witnessConstraint QSFact = Dict
    witnessConstraint QSLiteral = Dict

instance WitnessConstraint (WitnessConstraint Show) QSchema where
    witnessConstraint QSProperty = Dict
    witnessConstraint QSModelCount = Dict
    witnessConstraint QSFact = Dict
    witnessConstraint QSLiteral = Dict
