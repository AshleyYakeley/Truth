module Pinafore.Language.Type where

import Shapes

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

data PositiveType (vars :: [*]) (t :: *) where
    FunctionPositiveType :: NegativeType vars a -> PositiveType vars b -> PositiveType vars (Func a b)
    SimplePositiveType :: PositiveType1 vars a -> PositiveType vars a

instance ExprShow (PositiveType vars t) where
    exprShow (FunctionPositiveType a b) = exprShowSingle a <> " -> " <> exprShow b
    exprShow (SimplePositiveType t) = exprShow t
    exprShowSingle (SimplePositiveType t) = exprShowSingle t
    exprShowSingle t = "(" <> exprShow t <> ")"

data NegativeType (vars :: [*]) (t :: *) where
    FunctionNegativeType :: PositiveType vars a -> NegativeType vars b -> NegativeType vars (Func a b)
    SimpleNegativeType :: NegativeType1 vars a -> NegativeType vars a

instance ExprShow (NegativeType vars t) where
    exprShow (FunctionNegativeType a b) = exprShowSingle a <> " -> " <> exprShow b
    exprShow (SimpleNegativeType t) = exprShow t
    exprShowSingle (SimpleNegativeType t) = exprShowSingle t
    exprShowSingle t = "(" <> exprShow t <> ")"

data PositiveType1 (vars :: [*]) (t :: *) where
    OrPositiveType1 :: PositiveType2 vars a -> PositiveType1 vars b -> PositiveType1 vars (OrType a b)
    BottomPositiveType1 :: PositiveType1 vars None

instance ExprShow (PositiveType1 vars t) where
    exprShow BottomPositiveType1 = "Bottom"
    exprShow (OrPositiveType1 t BottomPositiveType1) = exprShow t
    exprShow (OrPositiveType1 t tr) = exprShowSingle t <> " | " <> exprShow tr
    exprShowSingle (OrPositiveType1 t BottomPositiveType1) = exprShowSingle t
    exprShowSingle t = "(" <> exprShow t <> ")"

data NegativeType1 (vars :: [*]) (t :: *) where
    AndNegativeType1 :: NegativeType2 vars a -> NegativeType1 vars b -> NegativeType1 vars (AndType a b)
    TopNegativeType1 :: NegativeType1 vars ()

instance ExprShow (NegativeType1 vars t) where
    exprShow TopNegativeType1 = "Top"
    exprShow (AndNegativeType1 t TopNegativeType1) = exprShow t
    exprShow (AndNegativeType1 t tr) = exprShowSingle t <> " & " <> exprShow tr
    exprShowSingle (AndNegativeType1 t TopNegativeType1) = exprShowSingle t
    exprShowSingle t = "(" <> exprShow t <> ")"

data PositiveType2 (vars :: [*]) (t :: *) where
    ListPositiveType2 :: PositiveType vars t -> PositiveType2 vars [t]
    GroundPositiveType2 :: GroundType t -> PositiveType2 vars t
    VarPositiveType2 :: ListElementWitness vars t -> PositiveType2 vars t
    RecPositiveType2 :: PositiveType (t ': vars) t -> PositiveType2 vars t

instance ExprShow (PositiveType2 vars t) where
    exprShow (RecPositiveType2 t) = "rec" <> " " <> "var" <> " = " <> exprShow t -- NYI
    exprShow t = exprShowSingle t
    exprShowSingle (ListPositiveType2 t) = "[" <> exprShow t <> "]"
    exprShowSingle (GroundPositiveType2 t) = exprShowSingle t
    exprShowSingle (VarPositiveType2 _v) = "var" -- NYI
    exprShowSingle t = "(" <> exprShow t <> ")"

data NegativeType2 (vars :: [*]) (t :: *) where
    ListNegativeType2 :: NegativeType vars t -> NegativeType2 vars [t]
    GroundNegativeType2 :: GroundType t -> NegativeType2 vars t
    VarNegativeType2 :: ListElementWitness vars t -> NegativeType2 vars t

instance ExprShow (NegativeType2 vars t) where
    exprShowSingle (ListNegativeType2 t) = "[" <> exprShow t <> "]"
    exprShowSingle (GroundNegativeType2 t) = exprShowSingle t
    exprShowSingle (VarNegativeType2 _v) = "var" -- NYI

data GroundType t where
    IntGroundType :: GroundType Int
    TextGroundType :: GroundType Text

instance ExprShow (GroundType t) where
    exprShowSingle IntGroundType = "Int"
    exprShowSingle TextGroundType = "Text"

class IsSubtype w where
    isSubtype :: w a -> w b -> Maybe (a -> b)

instance IsSubtype GroundType where
    isSubtype _ _ = Nothing
--class IsVarsSubtype wv where
 --   isVarsSubtype ::
