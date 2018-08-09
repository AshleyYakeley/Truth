{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Sealed where

import Language.Expression.Expression
import Language.Expression.NameWit
import Shapes

data SealedExpression nw vw tw =
    forall t. MkSealedExpression (tw t)
                                 (NameTypeExpression nw vw t)

type ApplyWitness m tw = forall r f a. tw f -> tw a -> (forall fa. tw fa -> (f -> a -> fa) -> m r) -> m r

applySealedExpression ::
       Monad m
    => ApplyWitness m tw
    -> SealedExpression nw vw tw
    -> SealedExpression nw vw tw
    -> m (SealedExpression nw vw tw)
applySealedExpression appf (MkSealedExpression tf exprf) (MkSealedExpression ta expra) =
    appf tf ta $ \tfa fafa -> return $ MkSealedExpression tfa $ liftA2 fafa exprf expra

constSealedExpression :: tw t -> t -> SealedExpression nw vw tw
constSealedExpression twt t = MkSealedExpression twt $ pure t

evalSealedExpression :: (MonadFail m, AllWitnessConstraint Show nw) => SealedExpression nw vw tw -> m (Any tw)
evalSealedExpression (MkSealedExpression twa expr) = do
    a <- evalExpression expr
    return $ MkAny twa a

varSealedExpression :: nw n -> vw n t -> tw t -> SealedExpression nw vw tw
varSealedExpression n vwt twt = MkSealedExpression twt $ varNameTypeExpression n vwt

newtype TypeChecker m w1 w2 =
    MkTypeChecker (forall a b. w1 a -> w2 b -> m (a -> b))

letSealedExpression ::
       (TestEquality nw, Monad m)
    => TypeJoiner m (vw n)
    -> TypeChecker m tw (vw n)
    -> nw n
    -> SealedExpression nw vw tw
    -> SealedExpression nw vw tw
    -> m (SealedExpression nw vw tw)
letSealedExpression joiner (MkTypeChecker checker) name (MkSealedExpression twe expre) (MkSealedExpression twb exprb) =
    abstractNTExpression joiner name exprb $ \vwt exprtb -> do
        et <- checker twe vwt
        return $ MkSealedExpression twb $ (\tb e -> tb $ et e) <$> exprtb <*> expre

type AbstractWitness m vw tw = forall r a b. vw a -> tw b -> (forall ab. tw ab -> ((a -> b) -> ab) -> m r) -> m r

abstractSealedExpression ::
       (TestEquality nw, Monad m)
    => TypeJoiner m (vw n)
    -> AbstractWitness m (vw n) tw
    -> nw n
    -> SealedExpression nw vw tw
    -> m (SealedExpression nw vw tw)
abstractSealedExpression joiner absWitness name (MkSealedExpression twb exprb) =
    abstractNTExpression joiner name exprb $ \vwt exprtb ->
        absWitness vwt twb $ \twtb conv -> return $ MkSealedExpression twtb $ fmap conv exprtb

type instance Element (SealedExpression nw vw ((:~:) val)) = val

instance MonoFunctor (SealedExpression nw vw ((:~:) val)) where
    omap ab (MkSealedExpression Refl expr) = MkSealedExpression Refl $ fmap ab expr

instance MonoPointed (SealedExpression nw vw ((:~:) val)) where
    opoint = constSealedExpression Refl

instance MonoApplicative (SealedExpression nw vw ((:~:) val)) where
    oliftA2 appf vexpr bexpr = runIdentity $ applySealedExpression (\Refl Refl cont -> cont Refl appf) vexpr bexpr
    osequenceA conv exprs =
        MkSealedExpression Refl $ fmap conv $ sequenceA $ fmap (\(MkSealedExpression Refl expr) -> expr) exprs
