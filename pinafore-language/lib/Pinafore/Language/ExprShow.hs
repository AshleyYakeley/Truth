module Pinafore.Language.ExprShow where

import Language.Expression.Common
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
