{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Sealed where

import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Renamer
import Shapes

data SealedExpression name vw tw =
    forall t. MkSealedExpression (tw t)
                                 (NamedExpression name vw t)

renameSealedExpression ::
       (Renamer rn, Monad m)
    => SealedExpression name (RenamerTSNegWitness rn) (RenamerTSPosWitness rn)
    -> rn m (SealedExpression name (RenamerTSNegWitness rn) (RenamerTSPosWitness rn))
renameSealedExpression (MkSealedExpression twt expr) =
    withTransConstraintTM @Monad $
    namespace $
    withTransConstraintTM @Monad $ do
        expr' <- renameExpression expr
        renameTSPosWitness twt $ \twt' bij -> return $ MkSealedExpression twt' $ fmap (biForwards bij) expr'

constSealedExpression :: AnyValue tw -> SealedExpression name vw tw
constSealedExpression (MkAnyValue twt t) = MkSealedExpression twt $ pure t

evalSealedExpression :: (MonadFail m, Show name) => SealedExpression name vw tw -> m (AnyValue tw)
evalSealedExpression (MkSealedExpression twa expr) = do
    a <- evalExpression expr
    return $ MkAnyValue twa a

varSealedExpression :: name -> vw tv -> tw tt -> (tv -> tt) -> SealedExpression name vw tw
varSealedExpression n vwt twt conv = MkSealedExpression twt $ fmap conv $ varNamedExpression n vwt

sealedExpressionFreeNames :: SealedExpression name vw tw -> [name]
sealedExpressionFreeNames (MkSealedExpression _ expr) = namedExpressionFreeNames expr

type instance Element (SealedExpression name vw ((:~:) val)) = val

instance MonoFunctor (SealedExpression name vw ((:~:) val)) where
    omap ab (MkSealedExpression Refl expr) = MkSealedExpression Refl $ fmap ab expr

instance MonoPointed (SealedExpression name vw ((:~:) val)) where
    opoint p = constSealedExpression $ MkAnyValue Refl p

instance MonoApplicative (SealedExpression name vw ((:~:) val)) where
    oliftA2 appf (MkSealedExpression Refl vexpr) (MkSealedExpression Refl bexpr) =
        MkSealedExpression Refl $ appf <$> vexpr <*> bexpr
    osequenceA conv exprs =
        MkSealedExpression Refl $ fmap conv $ sequenceA $ fmap (\(MkSealedExpression Refl expr) -> expr) exprs
