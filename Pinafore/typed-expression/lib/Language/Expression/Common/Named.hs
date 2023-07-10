{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Named where

import Language.Expression.Common.Error
import Language.Expression.Common.Expression
import Language.Expression.Common.NameWit
import Language.Expression.Common.Witness
import Language.Expression.Common.WitnessMappable
import Shapes

type UnitType :: Type -> Type -> Type
data UnitType a b where
    MkUnitType :: a -> UnitType a ()

instance Eq a => TestEquality (UnitType a) where
    testEquality (MkUnitType a1) (MkUnitType a2)
        | a1 == a2 = Just Refl
    testEquality _ _ = Nothing

instance Show a => Show (UnitType a b) where
    show (MkUnitType a) = show a

instance Show a => AllConstraint Show (UnitType a) where
    allConstraint = Dict

type UnitType' :: (Type -> Type) -> Type -> Type -> Type
data UnitType' a b c where
    MkUnitType' :: a c -> UnitType' a () c

instance Show (a c) => Show (UnitType' a b c) where
    show (MkUnitType' ac) = show ac

instance AllConstraint Show a => AllConstraint Show (UnitType' a b) where
    allConstraint :: forall (t :: Type). Dict (Show (UnitType' a b t))
    allConstraint =
        case allConstraint @_ @_ @Show @a @t of
            Dict -> Dict

instance AllConstraint Show a => AllConstraint (AllConstraint Show) (UnitType' a) where
    allConstraint = Dict

type NameWitness name w = NameTypeWitness (UnitType name) (UnitType' w)

pattern MkNameWitness :: name -> w t -> NameWitness name w t

pattern MkNameWitness name wit =
        MkNameTypeWitness (MkUnitType name) (MkUnitType' wit)

{-# COMPLETE MkNameWitness #-}

type NamedExpression :: Type -> (Type -> Type) -> Type -> Type
type NamedExpression name w = NameTypeExpression (UnitType name) (UnitType' w)

instance WitnessMappable poswit negwit (NamedExpression name negwit a) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \case
            ClosedExpression a -> pure $ ClosedExpression a
            OpenExpression (MkNameWitness name tt) expr -> do
                tt' <- unEndoM mapNeg tt
                expr' <- unEndoM (mapWitnessesM mapPos mapNeg) expr
                pure $ OpenExpression (MkNameWitness name tt') expr'

namedExpressionFreeNames :: NamedExpression name vw a -> [name]
namedExpressionFreeNames expr = expressionFreeWitnesses (\(MkNameWitness n _) -> n) expr

substituteExpression :: WitnessSubstitution Type vw1 vw2 -> NamedExpression name vw1 a -> NamedExpression name vw2 a
substituteExpression _ (ClosedExpression a) = ClosedExpression a
substituteExpression witmap@(MkWitnessConvert wm) (OpenExpression (MkNameWitness name wt) expr) =
    wm wt $ \wt' bij ->
        OpenExpression (MkNameWitness name wt') $
        fmap (\ta t2 -> ta $ isoBackwards bij t2) $ substituteExpression witmap expr

varNamedExpression :: name -> vw t -> NamedExpression name vw t
varNamedExpression n t = varNameTypeExpression (MkUnitType n) (MkUnitType' t)

type NamedExpressionError name w = ExpressionError (NameWitness name w)
