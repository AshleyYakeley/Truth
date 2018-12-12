module Pinafore.Language.Literal where

import Pinafore.Base
import Pinafore.Language.Show
import Shapes

class IsSubtype w where
    isSubtype :: w a -> w b -> Maybe (a -> b)

data LiteralType (t :: Type) where
    LiteralLiteralType :: LiteralType Literal
    UnitLiteralType :: LiteralType ()
    TextLiteralType :: LiteralType Text
    NumberLiteralType :: LiteralType Number
    BooleanLiteralType :: LiteralType Bool
    --NoneLiteralType :: LiteralType None

instance TestEquality LiteralType where
    testEquality LiteralLiteralType LiteralLiteralType = Just Refl
    testEquality UnitLiteralType UnitLiteralType = Just Refl
    testEquality TextLiteralType TextLiteralType = Just Refl
    testEquality NumberLiteralType NumberLiteralType = Just Refl
    testEquality BooleanLiteralType BooleanLiteralType = Just Refl
    --testEquality NoneLiteralType NoneLiteralType = Just Refl
    testEquality _ _ = Nothing

instance ExprShow (LiteralType t) where
    exprShowPrec LiteralLiteralType = ("Literal", 0)
    exprShowPrec UnitLiteralType = ("()", 0)
    exprShowPrec TextLiteralType = ("Text", 0)
    exprShowPrec NumberLiteralType = ("Number", 0)
    exprShowPrec BooleanLiteralType = ("Boolean", 0)
    --exprShowPrec NoneLiteralType = ("None", 0)

instance IsSubtype LiteralType where
    isSubtype LiteralLiteralType LiteralLiteralType = return id
    isSubtype TextLiteralType LiteralLiteralType = return toLiteral
    isSubtype NumberLiteralType LiteralLiteralType = return toLiteral
    isSubtype BooleanLiteralType LiteralLiteralType = return toLiteral
    isSubtype UnitLiteralType LiteralLiteralType = return toLiteral
    isSubtype TextLiteralType TextLiteralType = return id
    isSubtype NumberLiteralType NumberLiteralType = return id
    isSubtype BooleanLiteralType BooleanLiteralType = return id
    isSubtype UnitLiteralType UnitLiteralType = return id
    --isSubtype NoneLiteralType _ = return never
    isSubtype _ _ = Nothing

literalTypeAsLiteral :: LiteralType t -> Dict (AsLiteral t)
literalTypeAsLiteral LiteralLiteralType = Dict
literalTypeAsLiteral UnitLiteralType = Dict
literalTypeAsLiteral TextLiteralType = Dict
literalTypeAsLiteral NumberLiteralType = Dict
literalTypeAsLiteral BooleanLiteralType = Dict

--literalTypeAsLiteral NoneLiteralType = Dict
instance Representative LiteralType where
    getRepWitness LiteralLiteralType = Dict
    getRepWitness UnitLiteralType = Dict
    getRepWitness TextLiteralType = Dict
    getRepWitness NumberLiteralType = Dict
    getRepWitness BooleanLiteralType = Dict
    --getRepWitness NoneLiteralType = Dict

instance Is LiteralType Literal where
    representative = LiteralLiteralType

instance Is LiteralType () where
    representative = UnitLiteralType

instance Is LiteralType Text where
    representative = TextLiteralType

instance Is LiteralType Number where
    representative = NumberLiteralType

instance Is LiteralType Bool where
    representative = BooleanLiteralType
--instance Is LiteralType None where
--    representative = NoneLiteralType
