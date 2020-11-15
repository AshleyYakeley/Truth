module Language.Expression.Common.Named where

import Language.Expression.Common.Expression
import Language.Expression.Common.NameWit
import Language.Expression.Common.Pattern
import Language.Expression.Common.Witness
import Language.Expression.Common.WitnessMappable
import Shapes

data UnitType a b where
    MkUnitType :: a -> UnitType a ()

instance Eq a => TestEquality (UnitType a) where
    testEquality (MkUnitType a1) (MkUnitType a2)
        | a1 == a2 = Just Refl
    testEquality _ _ = Nothing

instance Show a => Show (UnitType a b) where
    show (MkUnitType a) = show a

instance Show a => AllWitnessConstraint Show (UnitType a) where
    allWitnessConstraint = Dict

data UnitType' a b c where
    MkUnitType' :: a c -> UnitType' a () c

type NameWitness name w = NameTypeWitness (UnitType name) (UnitType' w)

pattern MkNameWitness :: name -> w t -> NameWitness name w t

pattern MkNameWitness name wit =
        MkNameTypeWitness (MkUnitType name) (MkUnitType' wit)

{-# COMPLETE MkNameWitness #-}

type NamedExpression name w = NameTypeExpression (UnitType name) (UnitType' w)

instance WitnessMappable poswit negwit (NamedExpression name negwit a) where
    mapWitnessesM _ _ (ClosedExpression a) = return $ ClosedExpression a
    mapWitnessesM mapPos mapNeg (OpenExpression (MkNameWitness name tt) expr) = do
        tt' <- mapNeg tt
        expr' <- mapWitnessesM mapPos mapNeg expr
        return $ OpenExpression (MkNameWitness name tt') expr'

instance WitnessMappable poswit negwit (NamedPattern name poswit a b) where
    mapWitnessesM _ _ (ClosedPattern a) = return $ ClosedPattern a
    mapWitnessesM mapPos mapNeg (OpenPattern (MkNameWitness name tt) pat) = do
        tt' <- mapPos tt
        pat' <- mapWitnessesM mapPos mapNeg pat
        return $ OpenPattern (MkNameWitness name tt') pat'

namedExpressionFreeNames :: NamedExpression name vw a -> [name]
namedExpressionFreeNames expr = expressionFreeWitnesses (\(MkNameWitness n _) -> n) expr

substituteExpression :: WitnessSubstitution Type vw1 vw2 -> NamedExpression name vw1 a -> NamedExpression name vw2 a
substituteExpression _ (ClosedExpression a) = ClosedExpression a
substituteExpression witmap@(MkWitnessMap wm) (OpenExpression (MkNameWitness name wt) expr) =
    wm wt $ \wt' bij ->
        OpenExpression (MkNameWitness name wt') $
        fmap (\ta t2 -> ta $ isoBackwards bij t2) $ substituteExpression witmap expr

varNamedExpression :: name -> vw t -> NamedExpression name vw t
varNamedExpression n t = varNameTypeExpression (MkUnitType n) (MkUnitType' t)

type NamedPattern name w = NameTypePattern (UnitType name) (UnitType' w)

patternNames :: NamedPattern name vw q a -> [name]
patternNames = patternFreeWitnesses $ \(MkNameTypeWitness (MkUnitType name) _) -> name

substitutePattern :: WitnessSubstitution Type vw1 vw2 -> NamedPattern name vw1 q a -> NamedPattern name vw2 q a
substitutePattern _ (ClosedPattern a) = ClosedPattern a
substitutePattern witmap@(MkWitnessMap wm) (OpenPattern (MkNameWitness name wt) pat) =
    wm wt $ \wt' bij ->
        OpenPattern (MkNameWitness name wt') $ fmap (\(t, a) -> (isoForwards bij t, a)) $ substitutePattern witmap pat

varNamedPattern :: name -> vw t -> NamedPattern name vw t ()
varNamedPattern n t = varNameTypePattern (MkUnitType n) (MkUnitType' t)
