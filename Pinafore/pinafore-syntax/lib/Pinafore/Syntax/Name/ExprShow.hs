module Pinafore.Syntax.Name.ExprShow where

import Data.Shim
import Language.Expression.Common
import Language.Expression.TypeSystem
import Pinafore.Base
import Shapes

import Pinafore.Syntax.Name.FullName
import Pinafore.Syntax.Name.FullNameRef
import Pinafore.Syntax.Name.ImplicitName
import Pinafore.Syntax.Name.Name
import Pinafore.Syntax.Name.NamedText
import Pinafore.Syntax.Name.Namespace
import Pinafore.Syntax.Name.NamespaceRef
import Pinafore.Syntax.Name.PrecNamedText

class ExprShow t where
    exprShowPrec :: t -> PrecNamedText
    default exprShowPrec :: ShowNamedText t => t -> PrecNamedText
    exprShowPrec x = namedTextToPrec $ showNamedText x

exprPrecShow :: ExprShow t => Int -> t -> NamedText
exprPrecShow c t = precNamedText c $ exprShowPrec t

exprShow :: ExprShow t => t -> NamedText
exprShow = exprPrecShow maxBound

exprShowShow :: ExprShow t => t -> String
exprShowShow x = unpack $ toText $ exprShow x

instance ExprShow t => ExprShow (Maybe t) where
    exprShowPrec Nothing = ""
    exprShowPrec (Just t) = exprShowPrec t

instance ExprShow Name

instance ExprShow Namespace

instance ExprShow NamespaceRef

instance ExprShow FullName

instance ExprShow FullNameRef

instance ExprShow ImplicitName

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
    ListTypeExprShow (_ ': tt) = PrecNamedText -> ListTypeExprShow tt

instance ExprShow (TypeVar tv) where
    exprShowPrec (MkTypeVar v) = exprShowPrec v

instance (ExprShow name, ExprShow (w t)) => ExprShow (NameWitness name w t) where
    exprShowPrec (MkNameWitness name wt) = namedTextPrec 2 $ exprShow name <> ": " <> exprShow wt

instance (forall t. ExprShow (w t)) => ExprShow (Expression w a) where
    exprShowPrec expr = namedTextPrec 3 $ mconcat $ sort $ nub $ freeWitnesses (\w -> exprShow w <> ", ") expr

instance
    (ExprShow name, forall t. ExprShow (vw t), forall t. ExprShow (tw t)) =>
    ExprShow (SealedNamedExpression name vw tw)
    where
    exprShowPrec (MkSealedExpression twt expr) = namedTextPrec 3 $ exprShow expr <> exprShow twt

instance (forall t. ExprShow (w t)) => ExprShow (NonpolarArgument w sv a) where
    exprShowPrec = \case
        CoNonpolarArgument wt -> "+" <> exprShowPrec wt
        ContraNonpolarArgument wt -> "-" <> exprShowPrec wt
        RangeNonpolarArgument wp wq -> "(-" <> exprShowPrec wp <> "," <> exprShowPrec wq <> ")"
