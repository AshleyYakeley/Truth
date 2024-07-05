{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.SealedF where

import Language.Expression.Common.Expression
import Language.Expression.Common.Named
import Language.Expression.Common.Sealed
import Language.Expression.Common.WitnessMappable
import Shapes

data SealedFExpression (name :: Type) (vw :: Type -> Type) (tw :: Type -> Type) (f :: Type -> Type) =
    forall t. MkSealedFExpression (tw t)
                                  (NamedExpression name vw (f t))

constSealedFExpression :: SomeFor f tw -> SealedFExpression name vw tw f
constSealedFExpression (MkSomeFor twt t) = MkSealedFExpression twt $ pure t

mapSealedFExpression :: (f --> g) -> SealedFExpression name vw tw f -> SealedFExpression name vw tw g
mapSealedFExpression m (MkSealedFExpression twt expr) = MkSealedFExpression twt $ fmap m expr

applySealedFExpression ::
       SealedFExpression name vw tw ((->) a) -> NamedExpression name vw a -> SealedExpression name vw tw
applySealedFExpression (MkSealedFExpression twt exprar) expra = MkSealedExpression twt $ exprar <*> expra

toSealedFExpression :: Applicative f => SealedExpression name vw tw -> SealedFExpression name vw tw f
toSealedFExpression (MkSealedExpression twt expr) = MkSealedFExpression twt $ fmap pure expr

evalSealedFExpression ::
       (MonadThrow (NamedExpressionError name vw) m) => SealedFExpression name vw tw f -> m (SomeFor f tw)
evalSealedFExpression (MkSealedFExpression twa expr) = do
    a <- evalExpression expr
    return $ MkSomeFor twa a

varSealedFExpression :: name -> vw (f t) -> tw t -> SealedFExpression name vw tw f
varSealedFExpression n vwt twt = MkSealedFExpression twt $ varNamedExpression n vwt

sealedFExpressionFreeWitnesses :: (forall t. name -> vw t -> r) -> SealedFExpression name vw tw f -> [r]
sealedFExpressionFreeWitnesses f (MkSealedFExpression _ expr) =
    expressionFreeWitnesses (\(MkNameWitness n t) -> f n t) expr

sealedFExpressionFreeNames :: SealedFExpression name vw tw f -> [name]
sealedFExpressionFreeNames = sealedFExpressionFreeWitnesses $ \n _ -> n

sealedFExpressionType :: SealedFExpression name vw tw f -> Some tw
sealedFExpressionType (MkSealedFExpression t _) = MkSome t

type instance Element (SealedFExpression name vw ((:~:) val) f) = f val

instance MonoFunctor (SealedFExpression name vw ((:~:) val) f) where
    omap ab (MkSealedFExpression Refl expr) = MkSealedFExpression Refl $ fmap ab expr

instance MonoPointed (SealedFExpression name vw ((:~:) val) f) where
    opoint p = constSealedFExpression $ MkSomeFor Refl p

instance MonoApplicative (SealedFExpression name vw ((:~:) val) f) where
    oliftA2 appf (MkSealedFExpression Refl vexpr) (MkSealedFExpression Refl bexpr) =
        MkSealedFExpression Refl $ appf <$> vexpr <*> bexpr
    osequenceA conv exprs =
        MkSealedFExpression Refl $ fmap conv $ sequenceA $ fmap (\(MkSealedFExpression Refl expr) -> expr) exprs

instance WitnessMappable poswit negwit (SealedFExpression name negwit poswit f) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkSealedFExpression tt expr) -> do
            tt' <- unEndoM mapPos tt
            expr' <- unEndoM (mapWitnessesM mapPos mapNeg) expr
            pure $ MkSealedFExpression tt' expr'

instance (Show name, AllConstraint Show negwit, AllConstraint Show poswit) =>
             Show (SealedFExpression name negwit poswit f) where
    show (MkSealedFExpression t expr) = show expr <> " => " <> allShow t

typeFConstFExpression :: poswit t -> f t -> SealedFExpression name negwit poswit f
typeFConstFExpression tt t = MkSealedFExpression tt $ pure t
