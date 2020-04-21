module Pinafore.Language.TypeSystem.Show where

import Pinafore.Language.Name
import Prelude (Bounded(..))
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
    exprShowPrec (MkName n) = (n, 0)
