module Language.Expression.Common.Pattern.Expression where

import Data.Shim
import Language.Expression.Common.WitnessMappable
import Shapes

type ExpressionWitness :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data ExpressionWitness wit expr t where
    MkExpressionWitness :: wit t -> expr r -> ExpressionWitness wit expr (MeetType t r)

instance (forall t. WitnessMappable poswit negwit (wit t), forall t. WitnessMappable poswit negwit (expr t)) =>
             WitnessMappable poswit negwit (ExpressionWitness wit expr a) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkExpressionWitness w expr) ->
            liftA2
                MkExpressionWitness
                (unEndoM (mapWitnessesM mapPos mapNeg) w)
                (unEndoM (mapWitnessesM mapPos mapNeg) expr)

instance (AllConstraint Show wit, AllConstraint Show expr) => Show (ExpressionWitness wit expr t) where
    show (MkExpressionWitness w expr) = allShow w <> " " <> allShow expr

instance (AllConstraint Show wit, AllConstraint Show expr) => AllConstraint Show (ExpressionWitness wit expr) where
    allConstraint = Dict
