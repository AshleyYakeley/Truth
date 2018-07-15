module Pinafore.Language.Type where

import Pinafore.Literal
import Pinafore.Number
import Pinafore.Table (Point)
import Shapes

-- This is "soft" typing: it mostly represents types, but relies on coercing to and from a raw type for type variables.
type Func a b = a -> b

newtype OrType a b =
    MkOrType (Either a b)

newtype AndType a b =
    MkAndType (a, b)

class ExprShow t where
    exprShow :: t -> Text
    exprShow = exprShowSingle
    exprShowSingle :: t -> Text
    exprShowSingle t = "(" <> exprShow t <> ")"
    {-# MINIMAL exprShow | exprShowSingle #-}

data PositiveType (raw :: Type) (t :: Type) where
    FunctionPositiveType :: NegativeType raw a -> PositiveType raw b -> PositiveType raw (Func a b)
    SimplePositiveType :: PositiveType1 raw a -> PositiveType raw a

instance ExprShow (PositiveType raw t) where
    exprShow (FunctionPositiveType a b) = exprShowSingle a <> " -> " <> exprShow b
    exprShow (SimplePositiveType t) = exprShow t
    exprShowSingle (SimplePositiveType t) = exprShowSingle t
    exprShowSingle t = "(" <> exprShow t <> ")"

data NegativeType (raw :: Type) (t :: Type) where
    FunctionNegativeType :: PositiveType raw a -> NegativeType raw b -> NegativeType raw (Func a b)
    SimpleNegativeType :: NegativeType1 raw a -> NegativeType raw a

instance ExprShow (NegativeType raw t) where
    exprShow (FunctionNegativeType a b) = exprShowSingle a <> " -> " <> exprShow b
    exprShow (SimpleNegativeType t) = exprShow t
    exprShowSingle (SimpleNegativeType t) = exprShowSingle t
    exprShowSingle t = "(" <> exprShow t <> ")"

data PositiveType1 (raw :: Type) (t :: Type) where
    OrPositiveType1 :: PositiveType2 raw a -> PositiveType1 raw b -> PositiveType1 raw (OrType a b)
    BottomPositiveType1 :: PositiveType1 raw None

instance ExprShow (PositiveType1 raw t) where
    exprShow BottomPositiveType1 = "Bottom"
    exprShow (OrPositiveType1 t BottomPositiveType1) = exprShow t
    exprShow (OrPositiveType1 t tr) = exprShowSingle t <> " | " <> exprShow tr
    exprShowSingle (OrPositiveType1 t BottomPositiveType1) = exprShowSingle t
    exprShowSingle t = "(" <> exprShow t <> ")"

data NegativeType1 (raw :: Type) (t :: Type) where
    AndNegativeType1 :: NegativeType2 raw a -> NegativeType1 raw b -> NegativeType1 raw (AndType a b)
    TopNegativeType1 :: NegativeType1 raw ()

instance ExprShow (NegativeType1 raw t) where
    exprShow TopNegativeType1 = "Top"
    exprShow (AndNegativeType1 t TopNegativeType1) = exprShow t
    exprShow (AndNegativeType1 t tr) = exprShowSingle t <> " & " <> exprShow tr
    exprShowSingle (AndNegativeType1 t TopNegativeType1) = exprShowSingle t
    exprShowSingle t = "(" <> exprShow t <> ")"

data PositiveType2 (raw :: Type) (t :: Type) where
    ListPositiveType2 :: PositiveType raw t -> PositiveType2 raw [t]
    GroundPositiveType2 :: GroundType t -> PositiveType2 raw t
    VarPositiveType2 :: Int -> PositiveType2 raw raw

instance ExprShow (PositiveType2 raw t) where
    exprShow t = exprShowSingle t
    exprShowSingle (ListPositiveType2 t) = "[" <> exprShow t <> "]"
    exprShowSingle (GroundPositiveType2 t) = exprShowSingle t
    exprShowSingle (VarPositiveType2 _v) = "var" -- NYI

data NegativeType2 (raw :: Type) (t :: Type) where
    ListNegativeType2 :: NegativeType raw t -> NegativeType2 raw [t]
    GroundNegativeType2 :: GroundType t -> NegativeType2 raw t
    VarNegativeType2 :: Int -> NegativeType2 raw raw

instance ExprShow (NegativeType2 raw t) where
    exprShowSingle (ListNegativeType2 t) = "[" <> exprShow t <> "]"
    exprShowSingle (GroundNegativeType2 t) = exprShowSingle t
    exprShowSingle (VarNegativeType2 _v) = "var" -- NYI

data LiteralType (t :: Type) where
    LiteralLiteralType :: LiteralType Literal
    TextLiteralType :: LiteralType Text
    NumberLiteralType :: LiteralType Number

instance ExprShow (LiteralType t) where
    exprShowSingle LiteralLiteralType = "Literal"
    exprShowSingle TextLiteralType = "Text"
    exprShowSingle NumberLiteralType = "Number"

data GroundType (t :: Type) where
    LiteralGroundType :: LiteralType t -> GroundType t
    EntityGroundType :: Text -> GroundType Point

instance ExprShow (GroundType t) where
    exprShowSingle (LiteralGroundType t) = exprShowSingle t
    exprShowSingle (EntityGroundType t) = t

class IsSubtype w where
    isSubtype :: w a -> w b -> Maybe (a -> b)

instance IsSubtype GroundType where
    isSubtype _ _ = Nothing
