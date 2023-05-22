module Pinafore.Language.Name.ExprShow where

import Data.Shim
import Language.Expression.Common
import Pinafore.Language.Name.FullName
import Pinafore.Language.Name.FullNameRef
import Pinafore.Language.Name.Name
import Pinafore.Language.Name.NamedText
import Pinafore.Language.Name.Namespace
import Pinafore.Language.Name.NamespaceRef
import Pinafore.Language.Name.PrecNamedText
import Shapes

class ExprShow t where
    exprShowPrec :: t -> PrecNamedText
    default exprShowPrec :: ToNamedText t => t -> PrecNamedText
    exprShowPrec x = namedTextToPrec $ toNamedText x

exprPrecShow :: ExprShow t => Int -> t -> NamedText
exprPrecShow c t = precNamedText c $ exprShowPrec t

exprShow :: ExprShow t => t -> NamedText
exprShow = exprPrecShow maxBound

instance ExprShow t => ExprShow (Maybe t) where
    exprShowPrec Nothing = ""
    exprShowPrec (Just t) = exprShowPrec t

instance ExprShow Name

instance ExprShow Namespace

instance ExprShow NamespaceRef

instance ExprShow FullName

instance ExprShow FullNameRef

instance ExprShow (SymbolType name)

instance AllConstraint ExprShow w => ExprShow (Some w) where
    exprShowPrec (MkSome (wt :: w t)) =
        case allConstraint @_ @_ @ExprShow @w @t of
            Dict -> exprShowPrec wt

instance AllConstraint ExprShow w => ExprShow (ShimWit shim w a) where
    exprShowPrec (MkShimWit (wt :: w t) _) =
        case allConstraint @_ @_ @ExprShow @w @t of
            Dict -> exprShowPrec wt

type family ListTypeExprShow (dv :: [k]) :: Type where
    ListTypeExprShow '[] = PrecNamedText
    ListTypeExprShow (t ': tt) = PrecNamedText -> ListTypeExprShow tt

instance ExprShow (TypeVar tv) where
    exprShowPrec (MkTypeVar v) = exprShowPrec v
