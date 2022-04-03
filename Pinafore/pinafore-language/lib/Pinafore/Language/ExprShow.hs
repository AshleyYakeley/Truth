module Pinafore.Language.ExprShow where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Language.Name
import Shapes

class ExprShow t where
    exprShowPrec :: t -> (Text, Int)

precShow :: Int -> (Text, Int) -> Text
precShow c (s, p)
    | c < p = "(" <> s <> ")"
precShow _ (s, _) = s

exprPrecShow :: ExprShow t => Int -> t -> Text
exprPrecShow c t = precShow c $ exprShowPrec t

exprShow :: ExprShow t => t -> Text
exprShow = exprPrecShow maxBound

instance ExprShow t => ExprShow (Maybe t) where
    exprShowPrec Nothing = ("", 0)
    exprShowPrec (Just t) = exprShowPrec t

instance ExprShow Name where
    exprShowPrec n = (toText n, 0)

instance ExprShow ReferenceName where
    exprShowPrec n = (toText n, 0)

instance ExprShow (SymbolType name) where
    exprShowPrec n = (pack $ uVarName n, 0)

instance AllWitnessConstraint ExprShow w => ExprShow (AnyW w) where
    exprShowPrec (MkAnyW (wt :: w t)) =
        case allWitnessConstraint @_ @_ @ExprShow @w @t of
            Dict -> exprShowPrec wt

instance AllWitnessConstraint ExprShow w => ExprShow (ShimWit shim w a) where
    exprShowPrec (MkShimWit (wt :: w t) _) =
        case allWitnessConstraint @_ @_ @ExprShow @w @t of
            Dict -> exprShowPrec wt

type family ListTypeExprShow (dv :: [k]) :: Type where
    ListTypeExprShow '[] = (Text, Int)
    ListTypeExprShow (t ': tt) = (Text, Int) -> ListTypeExprShow tt

standardListTypeExprShow ::
       forall (dv :: [CCRVariance]). Is DolanVarianceType dv
    => Text
    -> ListTypeExprShow dv
standardListTypeExprShow = let
    sh :: forall (dv' :: [CCRVariance]). Int -> DolanVarianceType dv' -> Text -> ListTypeExprShow dv'
    sh i NilListType t = (t, i)
    sh _ (ConsListType _ lt) t = \ta -> sh 2 lt (t <> " " <> precShow 0 ta)
    in sh 0 $ representative @_ @_ @dv

nameIsInfix :: Name -> Bool
nameIsInfix n =
    case unpack n of
        (c:_)
            | isAlpha c -> False
        "[]" -> False
        _ -> True

data FixAssoc
    = AssocNone
    | AssocLeft
    | AssocRight
    deriving (Eq)

data Fixity = MkFixity
    { fixityAssoc :: FixAssoc
    , fixityPrec :: Int
    } deriving (Eq)
