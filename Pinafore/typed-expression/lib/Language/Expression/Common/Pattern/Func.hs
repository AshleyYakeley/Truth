module Language.Expression.Common.Pattern.Func where

import Language.Expression.Common.Expression
import Language.Expression.Common.Named
import Language.Expression.Common.Pattern.Pattern
import Shapes

type FuncPattern (patwit :: Type -> Type) (expwit :: Type -> Type)
     = Pattern patwit (PurityFunction Maybe (FunctionExpression expwit))

pureFuncPattern :: FunctionExpression expwit (a -> b) -> FuncPattern patwit expwit a b
pureFuncPattern expr = purePattern $ PureFunction expr

impureFuncPattern :: FunctionExpression expwit (a -> Maybe b) -> FuncPattern patwit expwit a b
impureFuncPattern expr = purePattern $ ImpureFunction expr

applyFuncPattern :: FuncPattern patwit expwit a b -> FunctionExpression expwit a -> FuncPattern patwit expwit () b
applyFuncPattern (MkPattern ww pf) ea = MkPattern ww $ applyPurityFunction pf ea

type NamedFuncPattern name poswit negwit = FuncPattern (NameWitness name poswit) (NameWitness name negwit)
