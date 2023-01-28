module Language.Expression.Common.Pattern.Expression where

import Data.Shim
import Language.Expression.Common.Named
import Language.Expression.Common.Pattern.Constructor
import Language.Expression.Common.Pattern.Pattern
import Language.Expression.Common.Pattern.Sealed
import Language.Expression.Common.WitnessTraversable
import Shapes

type ExpressionWitness :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data ExpressionWitness wit expr t where
    MkExpressionWitness :: wit t -> expr r -> ExpressionWitness wit expr (MeetType t r)

type NamedExpressionWitness name negwit = ExpressionWitness negwit (NamedExpression name negwit)

type SealedExpressionPattern (name :: Type) (patwit :: Type -> Type) (negwit :: Type -> Type)
     = SealedPattern patwit (NamedExpressionWitness name negwit)

liftExpressionShimWit ::
       (Applicative expr, JoinMeetCategory shim) => ShimWit shim wit t -> ShimWit shim (ExpressionWitness wit expr) t
liftExpressionShimWit (MkShimWit wtt conv) = MkShimWit (MkExpressionWitness wtt $ pure MkTopType) $ meetf conv termf

instance IsPatternWitness poswit patwit => WitnessTraversable poswit negwit (SealedExpressionPattern name patwit negwit) where
    traverseWitnessesM ::
           forall m. Applicative m
        => EndoM' m poswit
        -> EndoM' m negwit
        -> EndoM m (SealedExpressionPattern name patwit negwit)
    traverseWitnessesM mapPos mapNeg = let
        mapNeg' :: EndoM' m (ExpressionWitness negwit (NamedExpression name negwit))
        mapNeg' =
            MkEndoM $ \(MkExpressionWitness witt exprr) ->
                MkExpressionWitness <$> unEndoM mapNeg witt <*> unEndoM (traverseWitnessesM mapPos mapNeg) exprr
        in traverseWitnessesM @Type @poswit @(ExpressionWitness negwit (NamedExpression name negwit)) mapPos mapNeg'

varSealedExpressionPattern :: negwit t -> patwit (MeetType t ()) -> SealedExpressionPattern name patwit negwit
varSealedExpressionPattern twt vwt = varSealedPattern (MkExpressionWitness twt $ pure ()) vwt

anySealedExpressionPattern :: negwit t -> SealedExpressionPattern name patwit negwit
anySealedExpressionPattern twt = anySealedPattern $ MkExpressionWitness twt $ pure ()

type ExpressionPatternConstructor (name :: Type) (patwit :: Type -> Type) (poswit :: Type -> Type) (negwit :: Type -> Type)
     = PatternConstructor patwit poswit (NamedExpressionWitness name negwit)

toExpressionPatternConstructor ::
       PatternConstructor patwit poswit negwit -> ExpressionPatternConstructor name patwit poswit negwit
toExpressionPatternConstructor (MkPatternConstructor twt lt pat) =
    MkPatternConstructor (MkExpressionWitness twt $ pure MkTopType) lt $ pat . arr meet1

instance IsPatternWitness poswit patwit =>
             WitnessTraversable poswit negwit (ExpressionPatternConstructor name patwit poswit negwit) where
    traverseWitnessesM ::
           forall m. Applicative m
        => EndoM' m poswit
        -> EndoM' m negwit
        -> EndoM m (ExpressionPatternConstructor name patwit poswit negwit)
    traverseWitnessesM mapPos mapNeg = let
        mapNeg' :: EndoM' m (ExpressionWitness negwit (NamedExpression name negwit))
        mapNeg' =
            MkEndoM $ \(MkExpressionWitness witt exprr) ->
                MkExpressionWitness <$> unEndoM mapNeg witt <*> unEndoM (traverseWitnessesM mapPos mapNeg) exprr
        in traverseWitnessesM @Type @poswit @(ExpressionWitness negwit (NamedExpression name negwit)) mapPos mapNeg'
