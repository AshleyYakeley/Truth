{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Sealed where

import Language.Expression.Common.Error
import Language.Expression.Common.Expression
import Language.Expression.Common.Named
import Language.Expression.Common.WitnessTraversable
import Shapes

data SealedExpression (name :: Type) (vw :: Type -> Type) (tw :: Type -> Type) =
    forall t. MkSealedExpression (tw t)
                                 (NamedExpression name vw t)

constSealedExpression :: SomeOf tw -> SealedExpression name vw tw
constSealedExpression (MkSomeOf twt t) = MkSealedExpression twt $ pure t

evalSealedExpression ::
       (MonadThrow ExpressionError m, AllConstraint Show vw, Show name) => SealedExpression name vw tw -> m (SomeOf tw)
evalSealedExpression (MkSealedExpression twa expr) = do
    a <- evalExpression expr
    return $ MkSomeOf twa a

varSealedExpression :: name -> vw t -> tw t -> SealedExpression name vw tw
varSealedExpression n vwt twt = MkSealedExpression twt $ varNamedExpression n vwt

sealedExpressionFreeWitnesses :: (forall t. name -> vw t -> r) -> SealedExpression name vw tw -> [r]
sealedExpressionFreeWitnesses f (MkSealedExpression _ expr) =
    expressionFreeWitnesses (\(MkNameWitness n t) -> f n t) expr

sealedExpressionFreeNames :: SealedExpression name vw tw -> [name]
sealedExpressionFreeNames = sealedExpressionFreeWitnesses $ \n _ -> n

sealedExpressionType :: SealedExpression name vw tw -> Some tw
sealedExpressionType (MkSealedExpression t _) = MkSome t

type instance Element (SealedExpression name vw ((:~:) val)) = val

instance MonoFunctor (SealedExpression name vw ((:~:) val)) where
    omap ab (MkSealedExpression Refl expr) = MkSealedExpression Refl $ fmap ab expr

instance MonoPointed (SealedExpression name vw ((:~:) val)) where
    opoint p = constSealedExpression $ MkSomeOf Refl p

instance MonoApplicative (SealedExpression name vw ((:~:) val)) where
    oliftA2 appf (MkSealedExpression Refl vexpr) (MkSealedExpression Refl bexpr) =
        MkSealedExpression Refl $ appf <$> vexpr <*> bexpr
    osequenceA conv exprs =
        MkSealedExpression Refl $ fmap conv $ sequenceA $ fmap (\(MkSealedExpression Refl expr) -> expr) exprs

instance WitnessTraversable poswit negwit (SealedExpression name negwit poswit) where
    traverseWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkSealedExpression tt expr) -> do
            tt' <- unEndoM mapPos tt
            expr' <- unEndoM (traverseWitnessesM mapPos mapNeg) expr
            pure $ MkSealedExpression tt' expr'

instance (Show name, AllConstraint Show negwit, AllConstraint Show poswit) => Show (SealedExpression name negwit poswit) where
    show (MkSealedExpression t expr) = show expr <> " => " <> allShow t

typeFConstExpression :: poswit t -> t -> SealedExpression name negwit poswit
typeFConstExpression tt t = MkSealedExpression tt $ pure t
