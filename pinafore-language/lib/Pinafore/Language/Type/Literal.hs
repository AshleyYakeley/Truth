module Pinafore.Language.Type.Literal where

import Data.Time
import Pinafore.Base
import Pinafore.Language.Type.Show
import Shapes

data LiteralType (t :: Type) where
    LiteralLiteralType :: LiteralType Literal
    UnitLiteralType :: LiteralType ()
    TextLiteralType :: LiteralType Text
    NumberLiteralType :: LiteralType Number
    RationalLiteralType :: LiteralType SafeRational
    IntegerLiteralType :: LiteralType Integer
    BooleanLiteralType :: LiteralType Bool
    OrderingLiteralType :: LiteralType Ordering
    -- time
    TimeLiteralType :: LiteralType UTCTime
    DurationLiteralType :: LiteralType NominalDiffTime
    DateLiteralType :: LiteralType Day
    TimeOfDayLiteralType :: LiteralType TimeOfDay
    LocalTimeLiteralType :: LiteralType LocalTime

instance TestEquality LiteralType where
    testEquality LiteralLiteralType LiteralLiteralType = Just Refl
    testEquality UnitLiteralType UnitLiteralType = Just Refl
    testEquality TextLiteralType TextLiteralType = Just Refl
    testEquality NumberLiteralType NumberLiteralType = Just Refl
    testEquality RationalLiteralType RationalLiteralType = Just Refl
    testEquality IntegerLiteralType IntegerLiteralType = Just Refl
    testEquality BooleanLiteralType BooleanLiteralType = Just Refl
    testEquality OrderingLiteralType OrderingLiteralType = Just Refl
    testEquality TimeLiteralType TimeLiteralType = Just Refl
    testEquality DurationLiteralType DurationLiteralType = Just Refl
    testEquality DateLiteralType DateLiteralType = Just Refl
    testEquality TimeOfDayLiteralType TimeOfDayLiteralType = Just Refl
    testEquality LocalTimeLiteralType LocalTimeLiteralType = Just Refl
    testEquality _ _ = Nothing

instance ExprShow (LiteralType t) where
    exprShowPrec LiteralLiteralType = ("Literal", 0)
    exprShowPrec UnitLiteralType = ("()", 0)
    exprShowPrec TextLiteralType = ("Text", 0)
    exprShowPrec NumberLiteralType = ("Number", 0)
    exprShowPrec RationalLiteralType = ("Rational", 0)
    exprShowPrec IntegerLiteralType = ("Integer", 0)
    exprShowPrec BooleanLiteralType = ("Boolean", 0)
    exprShowPrec OrderingLiteralType = ("Ordering", 0)
    exprShowPrec TimeLiteralType = ("Time", 0)
    exprShowPrec DurationLiteralType = ("Duration", 0)
    exprShowPrec DateLiteralType = ("Date", 0)
    exprShowPrec TimeOfDayLiteralType = ("TimeOfDay", 0)
    exprShowPrec LocalTimeLiteralType = ("LocalTime", 0)

literalTypeAsLiteral :: LiteralType t -> Dict (AsLiteral t)
literalTypeAsLiteral LiteralLiteralType = Dict
literalTypeAsLiteral UnitLiteralType = Dict
literalTypeAsLiteral TextLiteralType = Dict
literalTypeAsLiteral NumberLiteralType = Dict
literalTypeAsLiteral RationalLiteralType = Dict
literalTypeAsLiteral IntegerLiteralType = Dict
literalTypeAsLiteral BooleanLiteralType = Dict
literalTypeAsLiteral OrderingLiteralType = Dict
literalTypeAsLiteral TimeLiteralType = Dict
literalTypeAsLiteral DurationLiteralType = Dict
literalTypeAsLiteral DateLiteralType = Dict
literalTypeAsLiteral TimeOfDayLiteralType = Dict
literalTypeAsLiteral LocalTimeLiteralType = Dict

instance Representative LiteralType where
    getRepWitness LiteralLiteralType = Dict
    getRepWitness UnitLiteralType = Dict
    getRepWitness TextLiteralType = Dict
    getRepWitness NumberLiteralType = Dict
    getRepWitness RationalLiteralType = Dict
    getRepWitness IntegerLiteralType = Dict
    getRepWitness BooleanLiteralType = Dict
    getRepWitness OrderingLiteralType = Dict
    getRepWitness TimeLiteralType = Dict
    getRepWitness DurationLiteralType = Dict
    getRepWitness DateLiteralType = Dict
    getRepWitness TimeOfDayLiteralType = Dict
    getRepWitness LocalTimeLiteralType = Dict

instance Is LiteralType Literal where
    representative = LiteralLiteralType

instance Is LiteralType () where
    representative = UnitLiteralType

instance Is LiteralType Text where
    representative = TextLiteralType

instance Is LiteralType Number where
    representative = NumberLiteralType

instance Is LiteralType SafeRational where
    representative = RationalLiteralType

instance Is LiteralType Integer where
    representative = IntegerLiteralType

instance Is LiteralType Bool where
    representative = BooleanLiteralType

instance Is LiteralType Ordering where
    representative = OrderingLiteralType

instance Is LiteralType UTCTime where
    representative = TimeLiteralType

instance Is LiteralType NominalDiffTime where
    representative = DurationLiteralType

instance Is LiteralType Day where
    representative = DateLiteralType

instance Is LiteralType TimeOfDay where
    representative = TimeOfDayLiteralType

instance Is LiteralType LocalTime where
    representative = LocalTimeLiteralType
