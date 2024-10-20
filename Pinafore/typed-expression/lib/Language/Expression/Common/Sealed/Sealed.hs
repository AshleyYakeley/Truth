{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Sealed.Sealed where

import Language.Expression.Common.Open
import Shapes

data SealedExpression (varw :: Type -> Type) (tw :: Type -> Type) =
    forall (t :: Type). MkSealedExpression (tw t)
                                           (Expression varw t)

type instance Element (SealedExpression varw ((:~:) val)) = val

instance MonoFunctor (SealedExpression varw ((:~:) val)) where
    omap ab (MkSealedExpression Refl expr) = MkSealedExpression Refl $ fmap ab expr

instance MonoPointed (SealedExpression varw ((:~:) val)) where
    opoint p = constSealedExpression $ MkSomeOf Refl p

instance MonoApplicative (SealedExpression varw ((:~:) val)) where
    oliftA2 appf (MkSealedExpression Refl vexpr) (MkSealedExpression Refl bexpr) =
        MkSealedExpression Refl $ appf <$> vexpr <*> bexpr
    osequenceA conv exprs =
        MkSealedExpression Refl $ fmap conv $ sequenceA $ fmap (\(MkSealedExpression Refl expr) -> expr) exprs

constSealedExpression :: SomeOf tw -> SealedExpression varw tw
constSealedExpression (MkSomeOf twt t) = MkSealedExpression twt $ pure t

typeFConstExpression :: poswit t -> t -> SealedExpression varw poswit
typeFConstExpression tt t = MkSealedExpression tt $ pure t

sealedExpressionType :: SealedExpression varw tw -> Some tw
sealedExpressionType (MkSealedExpression t _) = MkSome t

evalSealedExpression :: MonadThrow (ExpressionError varw) m => SealedExpression varw tw -> m (SomeOf tw)
evalSealedExpression (MkSealedExpression twa expr) = do
    a <- evalExpression expr
    return $ MkSomeOf twa a

evalSealedExpressionMaybe :: SealedExpression varw tw -> Maybe (SomeOf tw)
evalSealedExpressionMaybe (MkSealedExpression twa expr) = do
    a <- evalExpressionMaybe expr
    return $ MkSomeOf twa a

instance (AllConstraint Show varw, AllConstraint Show poswit) => Show (SealedExpression varw poswit) where
    show (MkSealedExpression t expr) = show expr <> " => " <> allShow t

type SealedNamedExpression (name :: Type) (vw :: Type -> Type) = SealedExpression (NameWitness name vw)

varSealedExpression :: name -> vw t -> tw t -> SealedNamedExpression name vw tw
varSealedExpression n vwt twt = MkSealedExpression twt $ varNamedExpression n vwt
