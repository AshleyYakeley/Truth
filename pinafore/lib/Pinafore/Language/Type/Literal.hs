module Pinafore.Language.Type.Literal where

import Data.Shim
import Data.Time
import Pinafore.Base
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type.Show
import Shapes

class IsSubtype w where
    isSubtype :: w a -> w b -> Maybe (PinaforePolyShim Type a b)

data LiteralType (t :: Type) where
    LiteralLiteralType :: LiteralType Literal
    UnitLiteralType :: LiteralType ()
    TextLiteralType :: LiteralType Text
    NumberLiteralType :: LiteralType Number
    RationalLiteralType :: LiteralType SafeRational
    IntegerLiteralType :: LiteralType Integer
    BooleanLiteralType :: LiteralType Bool
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
    exprShowPrec TimeLiteralType = ("Time", 0)
    exprShowPrec DurationLiteralType = ("Duration", 0)
    exprShowPrec DateLiteralType = ("Date", 0)
    exprShowPrec TimeOfDayLiteralType = ("TimeOfDay", 0)
    exprShowPrec LocalTimeLiteralType = ("LocalTime", 0)

nameToLiteralType :: Name -> Maybe (AnyW LiteralType)
nameToLiteralType "Literal" = Just $ MkAnyW LiteralLiteralType
nameToLiteralType "Text" = Just $ MkAnyW TextLiteralType
nameToLiteralType "Number" = Just $ MkAnyW NumberLiteralType
nameToLiteralType "Rational" = Just $ MkAnyW RationalLiteralType
nameToLiteralType "Integer" = Just $ MkAnyW IntegerLiteralType
nameToLiteralType "Boolean" = Just $ MkAnyW BooleanLiteralType
nameToLiteralType "Time" = Just $ MkAnyW TimeLiteralType
nameToLiteralType "Duration" = Just $ MkAnyW DurationLiteralType
nameToLiteralType "Date" = Just $ MkAnyW DateLiteralType
nameToLiteralType "TimeOfDay" = Just $ MkAnyW TimeOfDayLiteralType
nameToLiteralType "LocalTime" = Just $ MkAnyW LocalTimeLiteralType
nameToLiteralType _ = Nothing

instance IsSubtype LiteralType where
    isSubtype ta tb
        | Just Refl <- testEquality ta tb = return id
    isSubtype t LiteralLiteralType
        | Dict <- literalTypeAsLiteral t = return $ functionToShim "subtype" toLiteral
    isSubtype RationalLiteralType NumberLiteralType = return $ functionToShim "subtype" safeRationalToNumber
    isSubtype IntegerLiteralType NumberLiteralType =
        return $ functionToShim "subtype" $ safeRationalToNumber . integerToSafeRational
    isSubtype IntegerLiteralType RationalLiteralType = return $ functionToShim "subtype" integerToSafeRational
    isSubtype _ _ = Nothing

literalTypeAsLiteral :: LiteralType t -> Dict (AsLiteral t)
literalTypeAsLiteral LiteralLiteralType = Dict
literalTypeAsLiteral UnitLiteralType = Dict
literalTypeAsLiteral TextLiteralType = Dict
literalTypeAsLiteral NumberLiteralType = Dict
literalTypeAsLiteral RationalLiteralType = Dict
literalTypeAsLiteral IntegerLiteralType = Dict
literalTypeAsLiteral BooleanLiteralType = Dict
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
