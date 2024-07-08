module Language.Expression.Common.Pattern.TypeSystem where

import Language.Expression.Common.Pattern.Pattern
import Language.Expression.Common.Pattern.Sealed
import Shapes

class PatternTypeSystem (ts :: Type) where
    type TSPatVar ts :: Type -> Type
    type TSPatType ts :: Type -> Type

type TSPattern :: Type -> Type -> Type -> Type
type TSPattern ts = Pattern (TSPatVar ts)

type TSSealedPattern ts = SealedPattern (TSPatVar ts) (TSPatType ts)
