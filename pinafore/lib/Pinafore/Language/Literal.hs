module Pinafore.Language.Literal where

import Pinafore.Base
import Pinafore.Language.Show
import Shapes
import Shapes.Numeric

class IsSubtype w where
    isSubtype :: w a -> w b -> Maybe (a -> b)

data LiteralType (t :: Type) where
    LiteralLiteralType :: LiteralType Literal
    UnitLiteralType :: LiteralType ()
    TextLiteralType :: LiteralType Text
    NumberLiteralType :: LiteralType Number
    RationalLiteralType :: LiteralType Rational
    IntegerLiteralType :: LiteralType Integer
    BooleanLiteralType :: LiteralType Bool

instance TestEquality LiteralType where
    testEquality LiteralLiteralType LiteralLiteralType = Just Refl
    testEquality UnitLiteralType UnitLiteralType = Just Refl
    testEquality TextLiteralType TextLiteralType = Just Refl
    testEquality NumberLiteralType NumberLiteralType = Just Refl
    testEquality RationalLiteralType RationalLiteralType = Just Refl
    testEquality IntegerLiteralType IntegerLiteralType = Just Refl
    testEquality BooleanLiteralType BooleanLiteralType = Just Refl
    testEquality _ _ = Nothing

instance ExprShow (LiteralType t) where
    exprShowPrec LiteralLiteralType = ("Literal", 0)
    exprShowPrec UnitLiteralType = ("()", 0)
    exprShowPrec TextLiteralType = ("Text", 0)
    exprShowPrec NumberLiteralType = ("Number", 0)
    exprShowPrec RationalLiteralType = ("Rational", 0)
    exprShowPrec IntegerLiteralType = ("Integer", 0)
    exprShowPrec BooleanLiteralType = ("Boolean", 0)

instance IsSubtype LiteralType where
    isSubtype LiteralLiteralType LiteralLiteralType = return id
    isSubtype TextLiteralType LiteralLiteralType = return toLiteral
    isSubtype NumberLiteralType LiteralLiteralType = return toLiteral
    isSubtype RationalLiteralType LiteralLiteralType = return toLiteral
    isSubtype IntegerLiteralType LiteralLiteralType = return toLiteral
    isSubtype BooleanLiteralType LiteralLiteralType = return toLiteral
    isSubtype UnitLiteralType LiteralLiteralType = return toLiteral
    isSubtype TextLiteralType TextLiteralType = return id
    isSubtype NumberLiteralType NumberLiteralType = return id
    isSubtype RationalLiteralType RationalLiteralType = return id
    isSubtype IntegerLiteralType IntegerLiteralType = return id
    isSubtype RationalLiteralType NumberLiteralType = return ExactNumber
    isSubtype IntegerLiteralType NumberLiteralType = return $ ExactNumber . toRational
    isSubtype IntegerLiteralType RationalLiteralType = return toRational
    isSubtype BooleanLiteralType BooleanLiteralType = return id
    isSubtype UnitLiteralType UnitLiteralType = return id
    isSubtype _ _ = Nothing

literalTypeAsLiteral :: LiteralType t -> Dict (AsLiteral t)
literalTypeAsLiteral LiteralLiteralType = Dict
literalTypeAsLiteral UnitLiteralType = Dict
literalTypeAsLiteral TextLiteralType = Dict
literalTypeAsLiteral NumberLiteralType = Dict
literalTypeAsLiteral RationalLiteralType = Dict
literalTypeAsLiteral IntegerLiteralType = Dict
literalTypeAsLiteral BooleanLiteralType = Dict

instance Representative LiteralType where
    getRepWitness LiteralLiteralType = Dict
    getRepWitness UnitLiteralType = Dict
    getRepWitness TextLiteralType = Dict
    getRepWitness NumberLiteralType = Dict
    getRepWitness RationalLiteralType = Dict
    getRepWitness IntegerLiteralType = Dict
    getRepWitness BooleanLiteralType = Dict

instance Is LiteralType Literal where
    representative = LiteralLiteralType

instance Is LiteralType () where
    representative = UnitLiteralType

instance Is LiteralType Text where
    representative = TextLiteralType

instance Is LiteralType Number where
    representative = NumberLiteralType

instance Is LiteralType Rational where
    representative = RationalLiteralType

instance Is LiteralType Integer where
    representative = IntegerLiteralType

instance Is LiteralType Bool where
    representative = BooleanLiteralType
