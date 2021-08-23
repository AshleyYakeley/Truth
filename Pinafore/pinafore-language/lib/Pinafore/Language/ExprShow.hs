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

instance AllWitnessConstraint ExprShow w => ExprShow (AnyInKind w) where
    exprShowPrec (MkAnyInKind (wt :: w t)) =
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
