{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Sealed where

import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Pattern
import Language.Expression.Renamer
import Shapes

data SealedExpression name vw tw =
    forall t. MkSealedExpression (tw t)
                                 (NamedExpression name vw t)

renameSealedExpression ::
       (Renamer rn, Monad m)
    => SealedExpression name (RenamerNegWitness rn) (RenamerPosWitness rn)
    -> rn m (SealedExpression name (RenamerNegWitness rn) (RenamerPosWitness rn))
renameSealedExpression (MkSealedExpression twt expr) =
    withTransConstraintTM @Monad $
    namespace $
    withTransConstraintTM @Monad $ do
        expr' <- renameExpression expr
        renameTSPosWitness twt $ \twt' conv -> return $ MkSealedExpression twt' $ fmap conv expr'

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

data SealedPattern name vw tw =
    forall t. MkSealedPattern (tw t)
                              (NamedPattern name vw t ())

renameSealedPattern ::
       (Renamer rn, Monad m)
    => SealedPattern name (RenamerPosWitness rn) (RenamerNegWitness rn)
    -> rn m (SealedPattern name (RenamerPosWitness rn) (RenamerNegWitness rn))
renameSealedPattern (MkSealedPattern twt expr) =
    withTransConstraintTM @Monad $
    namespace $
    withTransConstraintTM @Monad $ do
        expr' <- renamePattern expr
        renameTSNegWitness twt $ \twt' conv -> return $ MkSealedPattern twt' $ contramap1Pattern conv expr'

varSealedPattern :: name -> tw t -> vw v -> (t -> v) -> SealedPattern name vw tw
varSealedPattern n twt vwt conv = MkSealedPattern twt $ varNamedPattern n vwt . arr conv

anySealedPattern :: tw t -> SealedPattern name vw tw
anySealedPattern twt = MkSealedPattern twt $ pure ()

data PatternConstructor name vw tw =
    forall t lt. MkPatternConstructor (tw t)
                                      (ListType vw lt)
                                      (NamedPattern name vw t (HList lt))

sealedPatternConstructor :: MonadFail m => PatternConstructor name vw tw -> m (SealedPattern name vw tw)
sealedPatternConstructor (MkPatternConstructor twt NilListType pat) = return $ MkSealedPattern twt pat
sealedPatternConstructor _ = fail "Not enough arguments to constructor in pattern"
