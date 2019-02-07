module Language.Expression.Named where

import Language.Expression.Expression
import Language.Expression.NameWit
import Language.Expression.Pattern
import Language.Expression.TypeF
import Language.Expression.TypeMappable
import Language.Expression.Witness
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

instance TypeMappable (->) poswit negwit (NamedExpression name negwit a) where
    mapTypesM _ _ (ClosedExpression a) = return $ ClosedExpression a
    mapTypesM mapPos mapNeg (OpenExpression (MkNameWitness name tt) expr) = do
        MkTypeF tt' conv <- mapNeg tt
        expr' <- mapTypesM mapPos mapNeg expr
        return $ OpenExpression (MkNameWitness name tt') $ fmap (\ta -> ta . conv) expr'

instance TypeMappable (->) poswit negwit (NamedPattern name poswit a b) where
    mapTypesM _ _ (ClosedPattern a) = return $ ClosedPattern a
    mapTypesM mapPos mapNeg (OpenPattern (MkNameWitness name tt) pat) = do
        MkTypeF tt' conv <- mapPos tt
        pat' <- mapTypesM mapPos mapNeg pat
        return $ OpenPattern (MkNameWitness name tt') $ fmap (\(t, b) -> (conv t, b)) pat'

namedExpressionFreeNames :: NamedExpression name vw a -> [name]
namedExpressionFreeNames expr = expressionFreeWitnesses (\(MkNameWitness n _) -> n) expr

substituteExpression :: WitnessSubstitution Type vw1 vw2 -> NamedExpression name vw1 a -> NamedExpression name vw2 a
substituteExpression _ (ClosedExpression a) = ClosedExpression a
substituteExpression witmap@(MkWitnessMap wm) (OpenExpression (MkNameWitness name wt) expr) =
    wm wt $ \wt' bij ->
        OpenExpression (MkNameWitness name wt') $
        fmap (\ta t2 -> ta $ biBackwards bij t2) $ substituteExpression witmap expr

varNamedExpression :: name -> vw t -> NamedExpression name vw t
varNamedExpression n t = varNameTypeExpression (MkUnitType n) (MkUnitType' t)

type NamedPattern name w = NameTypePattern (UnitType name) (UnitType' w)

substitutePattern :: WitnessSubstitution Type vw1 vw2 -> NamedPattern name vw1 q a -> NamedPattern name vw2 q a
substitutePattern _ (ClosedPattern a) = ClosedPattern a
substitutePattern witmap@(MkWitnessMap wm) (OpenPattern (MkNameWitness name wt) pat) =
    wm wt $ \wt' bij ->
        OpenPattern (MkNameWitness name wt') $ fmap (\(t, a) -> (biForwards bij t, a)) $ substitutePattern witmap pat

varNamedPattern :: name -> vw t -> NamedPattern name vw t ()
varNamedPattern n t = varNameTypePattern (MkUnitType n) (MkUnitType' t)
