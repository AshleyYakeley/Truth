module Language.Expression.Common.Pattern.Func where

import Language.Expression.Common.Open
import Language.Expression.Common.Pattern.Pattern
import Shapes

type FuncPattern (patwit :: Type -> Type) (expwit :: Type -> Type)
     = Pattern patwit (PurityFunction Maybe (Expression expwit))

pureFuncPattern :: Expression expwit (a -> b) -> FuncPattern patwit expwit a b
pureFuncPattern expr = purePattern $ PureFunction expr

impureFuncPattern :: Expression expwit (a -> Maybe b) -> FuncPattern patwit expwit a b
impureFuncPattern expr = purePattern $ ImpureFunction expr

applyFuncPattern :: FuncPattern patwit expwit a b -> Expression expwit a -> FuncPattern patwit expwit () b
applyFuncPattern (MkPattern ww pf) ea = MkPattern ww $ applyPurityFunction pf ea

type NamedFuncPattern name poswit negwit = FuncPattern (NameWitness name poswit) (NameWitness name negwit)
