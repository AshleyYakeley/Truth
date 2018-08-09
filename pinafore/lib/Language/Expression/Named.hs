module Language.Expression.Named where

import Language.Expression.Bindings
import Language.Expression.NameWit
import Language.Expression.Sealed
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

type NamedExpression name w = NameTypeExpression (UnitWitness name) (UnitWitness' w)

type SealedNamedExpression name vw tw = SealedExpression (UnitWitness name) (UnitWitness' vw) tw

nameJoiner :: Joiner w ta tb -> Joiner (UnitWitness' w ()) ta tb
nameJoiner (MkJoiner wtab conva convb) = MkJoiner (MkUnitWitness' wtab) conva convb

nameTypeJoiner :: Functor m => TypeJoiner m w -> TypeJoiner m (UnitWitness' w ())
nameTypeJoiner (MkTypeJoiner wt0 joinwit) =
    MkTypeJoiner (MkUnitWitness' wt0) $ \(MkUnitWitness' wta) (MkUnitWitness' wtb) -> fmap nameJoiner $ joinwit wta wtb

nameTypeChecker :: TypeChecker m w1 w2 -> TypeChecker m w1 (UnitWitness' w2 ())
nameTypeChecker (MkTypeChecker checker) = MkTypeChecker $ \w1a (MkUnitWitness' w2a) -> checker w1a w2a

nameAbstractWitness :: AbstractWitness m vw tw -> AbstractWitness m (UnitWitness' vw ()) tw
nameAbstractWitness absw (MkUnitWitness' vwa) = absw vwa

letSealedNamedExpression ::
       (Eq name, Monad m)
    => TypeJoiner m vw
    -> TypeChecker m tw vw
    -> name
    -> SealedNamedExpression name vw tw
    -> SealedNamedExpression name vw tw
    -> m (SealedNamedExpression name vw tw)
letSealedNamedExpression joiner checker name =
    letSealedExpression (nameTypeJoiner joiner) (nameTypeChecker checker) (MkUnitWitness name)

varSealedNameExpression :: name -> vw t -> tw t -> SealedNamedExpression name vw tw
varSealedNameExpression name vwt = varSealedExpression (MkUnitWitness name) (MkUnitWitness' vwt)

abstractSealedNamedExpression ::
       (Eq name, Monad m)
    => TypeJoiner m vw
    -> AbstractWitness m vw tw
    -> name
    -> SealedNamedExpression name vw tw
    -> m (SealedNamedExpression name vw tw)
abstractSealedNamedExpression joiner absw name =
    abstractSealedExpression (nameTypeJoiner joiner) (nameAbstractWitness absw) (MkUnitWitness name)

type NamedBindings name vw = Bindings (UnitWitness name) (UnitWitness' vw)

singleNamedBinding :: name -> SealedNamedExpression name vw tw -> NamedBindings name vw tw
singleNamedBinding name = singleBinding (MkUnitWitness name)

uncheckedBindingsLetSealedNamedExpression ::
       (Eq name, Monad m)
    => TypeJoiner m vw
    -> TypeChecker m tw vw
    -> NamedBindings name vw tw
    -> SealedNamedExpression name vw tw
    -> m (SealedNamedExpression name vw tw)
uncheckedBindingsLetSealedNamedExpression joiner checker =
    bindingsLetSealedExpression
        (\(MkUnitWitness _) -> nameTypeJoiner joiner)
        (\(MkUnitWitness _) -> nameTypeChecker checker)
