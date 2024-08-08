{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Sealed.SealedF where

import Language.Expression.Common.Open.Error
import Language.Expression.Common.Open.Expression
import Language.Expression.Common.Sealed.Sealed
import Language.Expression.Common.WitnessMappable
import Shapes

data SealedFExpression (varw :: Type -> Type) (tw :: Type -> Type) (f :: Type -> Type) =
    forall t. MkSealedFExpression (tw t)
                                  (Expression varw (f t))

constSealedFExpression :: SomeFor f tw -> SealedFExpression varw tw f
constSealedFExpression (MkSomeFor twt t) = MkSealedFExpression twt $ pure t

mapSealedFExpression :: (f --> g) -> SealedFExpression varw tw f -> SealedFExpression varw tw g
mapSealedFExpression m (MkSealedFExpression twt expr) = MkSealedFExpression twt $ fmap m expr

applySealedFExpression :: SealedFExpression varw tw ((->) a) -> Expression varw a -> SealedExpression varw tw
applySealedFExpression (MkSealedFExpression twt exprar) expra = MkSealedExpression twt $ exprar <*> expra

toSealedFExpression :: Applicative f => SealedExpression varw tw -> SealedFExpression varw tw f
toSealedFExpression (MkSealedExpression twt expr) = MkSealedFExpression twt $ fmap pure expr

evalSealedFExpression :: (MonadThrow (ExpressionError varw) m) => SealedFExpression varw tw f -> m (SomeFor f tw)
evalSealedFExpression (MkSealedFExpression twa expr) = do
    a <- evalExpression expr
    return $ MkSomeFor twa a

sealedFExpressionType :: SealedFExpression varw tw f -> Some tw
sealedFExpressionType (MkSealedFExpression t _) = MkSome t

type instance Element (SealedFExpression varw ((:~:) val) f) = f val

instance MonoFunctor (SealedFExpression varw ((:~:) val) f) where
    omap ab (MkSealedFExpression Refl expr) = MkSealedFExpression Refl $ fmap ab expr

instance MonoPointed (SealedFExpression varw ((:~:) val) f) where
    opoint p = constSealedFExpression $ MkSomeFor Refl p

instance MonoApplicative (SealedFExpression varw ((:~:) val) f) where
    oliftA2 appf (MkSealedFExpression Refl vexpr) (MkSealedFExpression Refl bexpr) =
        MkSealedFExpression Refl $ appf <$> vexpr <*> bexpr
    osequenceA conv exprs =
        MkSealedFExpression Refl $ fmap conv $ sequenceA $ fmap (\(MkSealedFExpression Refl expr) -> expr) exprs

instance (forall t. WitnessMappable poswit negwit (varw t)) =>
             WitnessMappable poswit negwit (SealedFExpression varw poswit f) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkSealedFExpression tt expr) -> do
            tt' <- unEndoM mapPos tt
            expr' <- unEndoM (mapWitnessesM mapPos mapNeg) expr
            pure $ MkSealedFExpression tt' expr'

instance (AllConstraint Show varw, AllConstraint Show poswit) => Show (SealedFExpression varw poswit f) where
    show (MkSealedFExpression t expr) = show expr <> " => " <> allShow t
