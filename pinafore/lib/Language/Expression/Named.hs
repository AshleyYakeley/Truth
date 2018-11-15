module Language.Expression.Named where

import Language.Expression.Expression
import Language.Expression.NameWit
import Language.Expression.Renamer
import Language.Expression.Witness
import Shapes

data UnitWitness a b where
    MkUnitWitness :: a -> UnitWitness a ()

instance Eq a => TestEquality (UnitWitness a) where
    testEquality (MkUnitWitness a1) (MkUnitWitness a2)
        | a1 == a2 = Just Refl
    testEquality _ _ = Nothing

instance Show a => Show (UnitWitness a b) where
    show (MkUnitWitness a) = show a

instance Show a => AllWitnessConstraint Show (UnitWitness a) where
    allWitnessConstraint = Dict

data UnitWitness' a b c where
    MkUnitWitness' :: a c -> UnitWitness' a () c

type NameWitness name w = NameTypeWitness (UnitWitness name) (UnitWitness' w)

pattern MkNameWitness :: name -> w t -> NameWitness name w t

pattern MkNameWitness name wit =
        MkNameTypeWitness (MkUnitWitness name) (MkUnitWitness' wit)

{-# COMPLETE MkNameWitness #-}

type NamedExpression name w = NameTypeExpression (UnitWitness name) (UnitWitness' w)

namedExpressionFreeNames :: NamedExpression name vw a -> [name]
namedExpressionFreeNames expr = expressionFreeWitnesses (\(MkNameWitness n _) -> n) expr

substituteExpression :: WitnessSubstitution Type vw1 vw2 -> NamedExpression name vw1 a -> NamedExpression name vw2 a
substituteExpression _ (ClosedExpression a) = ClosedExpression a
substituteExpression witmap@(MkWitnessMap wm) (OpenExpression (MkNameWitness name wt) expr) =
    wm wt $ \wt' bij ->
        OpenExpression (MkNameWitness name wt') $
        fmap (\ta t2 -> ta $ biBackwards bij t2) $ substituteExpression witmap expr

varNamedExpression :: name -> vw t -> NamedExpression name vw t
varNamedExpression n t = varNameTypeExpression (MkUnitWitness n) (MkUnitWitness' t)

renameExpression ::
       forall rn name m t. (Monad m, Renamer rn)
    => NamedExpression name (RenamerNegWitness rn) t
    -> RenamerNamespace rn (rn m) (NamedExpression name (RenamerNegWitness rn) t)
renameExpression (ClosedExpression a) =
    case hasTransConstraint @Monad @rn @m of
        Dict -> withTransConstraintTM @Monad $ return (ClosedExpression a)
renameExpression (OpenExpression (MkNameWitness name vw) expr) =
    case hasTransConstraint @Monad @rn @m of
        Dict ->
            withTransConstraintTM @Monad $
            renameNegWitness vw $ \vw' bij -> do
                expr' <- renameExpression expr
                return $ OpenExpression (MkNameWitness name vw') $ fmap (\va -> va . biBackwards bij) expr'
