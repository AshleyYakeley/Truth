module Pinafore.Language.Type where

import Shapes

type VK = [Type] -> Type

type family LangType (t :: *)

newtype K0 f (vars :: [*]) =
    MkK0 f

newtype K1 f a (vars :: [*]) =
    MkK1 (f (a vars))

newtype K2 f a b (vars :: [*]) =
    MkK2 (f (a vars) (b vars))

type Func = K2 (->)

newtype OrType a b (vars :: [*]) =
    MkOrType (Either (a vars) (b vars))

newtype AndType a b (vars :: [*]) =
    MkAndType (a vars, b vars)

newtype ListV a (vars :: [*]) =
    MkListV [a vars]

data PickV (vars :: [*]) (t :: *) (vars' :: [*]) where
    MkPickV :: t -> PickV vars t vars

class ExprShow t where
    exprShow :: t -> Text
    exprShow = exprShowSingle
    exprShowSingle :: t -> Text
    exprShowSingle t = "(" <> exprShow t <> ")"
    {-# MINIMAL exprShow | exprShowSingle #-}

data FirstVarT vars where
    MkFirstVarT :: t -> FirstVarT (t ': r)

data NextVarT n vars where
    MkNextVarT :: n vars -> NextVarT n (t ': vars)

{-
type TestId = Func FirstVarT FirstVarT

toTestId :: (forall a. a -> a) -> (forall vars. TestId vars)
toTestId f = MkK2 $ \(MkFirstVarT a) -> MkFirstVarT $ f a

fromTestId :: (forall vars. TestId vars) -> (forall a. a -> a)
fromTestId (MkK2 f) a =  case f (MkFirstVarT a) of
    MkFirstVarT fa -> fa


type TestV = Func (K2 (,) FirstVarT (NextVarT FirstVarT)) (K2 (,) (NextVarT FirstVarT) FirstVarT)

toTestV :: (forall a b. (a,b) -> (b,a)) -> (forall vars. TestV vars)
toTestV f = MkK2 $ \(MkK2 (MkFirstVarT a,MkNextVarT (MkFirstVarT b))) -> case f (a,b) of
    (b',a') -> (MkK2 (MkNextVarT (MkFirstVarT b'),MkFirstVarT a'))
-}
data PickWitness (z :: k) (s :: k -> k) (t :: k) where
    FirstPickWitness :: PickWitness z s z
    NextPickWitness :: PickWitness z s t -> PickWitness z s (s t)

type VarWitness = PickWitness FirstVarT NextVarT

--class IsTypeExpression (te :: [*] -> * -> *) where
--    typeExpressionInstantiate :: ListElementWitness vars t -> te vars a -> te vars' t -> te vars' a
data PositiveType (t :: VK) where
    FunctionPositiveType :: NegativeType a -> PositiveType b -> PositiveType (Func a b)
    SimplePositiveType :: PositiveType1 a -> PositiveType a

instance ExprShow (PositiveType t) where
    exprShow (FunctionPositiveType a b) = exprShowSingle a <> " -> " <> exprShow b
    exprShow (SimplePositiveType t) = exprShow t
    exprShowSingle (SimplePositiveType t) = exprShowSingle t
    exprShowSingle t = "(" <> exprShow t <> ")"

data NegativeType (t :: VK) where
    FunctionNegativeType :: PositiveType a -> NegativeType b -> NegativeType (Func a b)
    SimpleNegativeType :: NegativeType1 a -> NegativeType a

instance ExprShow (NegativeType t) where
    exprShow (FunctionNegativeType a b) = exprShowSingle a <> " -> " <> exprShow b
    exprShow (SimpleNegativeType t) = exprShow t
    exprShowSingle (SimpleNegativeType t) = exprShowSingle t
    exprShowSingle t = "(" <> exprShow t <> ")"

data PositiveType1 (t :: VK) where
    OrPositiveType1 :: PositiveType2 a -> PositiveType1 b -> PositiveType1 (OrType a b)
    BottomPositiveType1 :: PositiveType1 (Const None)

instance ExprShow (PositiveType1 t) where
    exprShow BottomPositiveType1 = "Bottom"
    exprShow (OrPositiveType1 t BottomPositiveType1) = exprShow t
    exprShow (OrPositiveType1 t tr) = exprShowSingle t <> " | " <> exprShow tr
    exprShowSingle (OrPositiveType1 t BottomPositiveType1) = exprShowSingle t
    exprShowSingle t = "(" <> exprShow t <> ")"

data NegativeType1 (t :: VK) where
    AndNegativeType1 :: NegativeType2 a -> NegativeType1 b -> NegativeType1 (AndType a b)
    TopNegativeType1 :: NegativeType1 (Const ())

instance ExprShow (NegativeType1 t) where
    exprShow TopNegativeType1 = "Top"
    exprShow (AndNegativeType1 t TopNegativeType1) = exprShow t
    exprShow (AndNegativeType1 t tr) = exprShowSingle t <> " & " <> exprShow tr
    exprShowSingle (AndNegativeType1 t TopNegativeType1) = exprShowSingle t
    exprShowSingle t = "(" <> exprShow t <> ")"

data PositiveType2 (t :: VK) where
    ListPositiveType2 :: PositiveType t -> PositiveType2 (ListV t)
    GroundPositiveType2 :: GroundType t -> PositiveType2 (Const t)
    VarPositiveType2 :: VarWitness t -> PositiveType2 t
    --RecPositiveType2 :: PositiveType (t ': vars) t -> PositiveType2 t

instance ExprShow (PositiveType2 t) where
    --exprShow (RecPositiveType2 t) = "rec" <> " " <> "var" <> " = " <> exprShow t -- NYI
    exprShow t = exprShowSingle t
    exprShowSingle (ListPositiveType2 t) = "[" <> exprShow t <> "]"
    exprShowSingle (GroundPositiveType2 t) = exprShowSingle t
    exprShowSingle (VarPositiveType2 _v) = "var" -- NYI
    -- exprShowSingle t = "(" <> exprShow t <> ")"

data NegativeType2 (t :: VK) where
    ListNegativeType2 :: NegativeType t -> NegativeType2 (ListV t)
    GroundNegativeType2 :: GroundType t -> NegativeType2 (Const t)
    VarNegativeType2 :: VarWitness t -> NegativeType2 t

instance ExprShow (NegativeType2 t) where
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
