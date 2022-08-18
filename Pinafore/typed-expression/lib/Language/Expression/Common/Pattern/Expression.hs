module Language.Expression.Common.Pattern.Expression where

import Data.Shim
import Language.Expression.Common.Named
import Language.Expression.Common.Pattern.Constructor
import Language.Expression.Common.Pattern.Sealed
import Language.Expression.Common.WitnessMappable
import Shapes

type ExpressionWitness :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data ExpressionWitness wit expr t where
    MkExpressionWitness :: wit t -> expr r -> ExpressionWitness wit expr (MeetType t r)

type NamedExpressionWitness name tw = ExpressionWitness tw (NamedExpression name tw)

type SealedExpressionPattern (name :: Type) (vw :: Type -> Type) (tw :: Type -> Type)
     = SealedPattern name vw (NamedExpressionWitness name tw)

liftExpressionShimWit ::
       (Applicative expr, JoinMeetCategory shim) => ShimWit shim wit t -> ShimWit shim (ExpressionWitness wit expr) t
liftExpressionShimWit (MkShimWit wtt conv) = MkShimWit (MkExpressionWitness wtt $ pure MkTopType) $ meetf conv termf

instance WitnessMappable poswit negwit (SealedExpressionPattern name poswit negwit) where
    mapWitnessesM ::
           forall m. Applicative m
        => (forall t. poswit t -> m (poswit t))
        -> (forall t. negwit t -> m (negwit t))
        -> SealedExpressionPattern name poswit negwit
        -> m (SealedExpressionPattern name poswit negwit)
    mapWitnessesM mapPos mapNeg spat = let
        mapNeg' ::
               forall t.
               ExpressionWitness negwit (NamedExpression name negwit) t
            -> m (ExpressionWitness negwit (NamedExpression name negwit) t)
        mapNeg' (MkExpressionWitness witt exprr) =
            MkExpressionWitness <$> mapNeg witt <*> mapWitnessesM mapPos mapNeg exprr
        in mapWitnessesM @Type @poswit @(ExpressionWitness negwit (NamedExpression name negwit)) mapPos mapNeg' spat

varSealedExpressionPattern :: name -> tw t -> vw (MeetType t ()) -> SealedExpressionPattern name vw tw
varSealedExpressionPattern n twt vwt = varSealedPattern n (MkExpressionWitness twt $ pure ()) vwt

anySealedExpressionPattern :: tw t -> SealedExpressionPattern name vw tw
anySealedExpressionPattern twt = anySealedPattern $ MkExpressionWitness twt $ pure ()

type ExpressionPatternConstructor (name :: Type) (vw :: Type -> Type) (tw :: Type -> Type)
     = PatternConstructor name vw (NamedExpressionWitness name tw)

toExpressionPatternConstructor :: PatternConstructor name vw tw -> ExpressionPatternConstructor name vw tw
toExpressionPatternConstructor (MkPatternConstructor twt lt pat) =
    MkPatternConstructor (MkExpressionWitness twt $ pure MkTopType) lt $ pat . arr meet1

instance WitnessMappable poswit negwit (ExpressionPatternConstructor name poswit negwit) where
    mapWitnessesM ::
           forall m. Applicative m
        => (forall t. poswit t -> m (poswit t))
        -> (forall t. negwit t -> m (negwit t))
        -> ExpressionPatternConstructor name poswit negwit
        -> m (ExpressionPatternConstructor name poswit negwit)
    mapWitnessesM mapPos mapNeg spat = let
        mapNeg' ::
               forall t.
               ExpressionWitness negwit (NamedExpression name negwit) t
            -> m (ExpressionWitness negwit (NamedExpression name negwit) t)
        mapNeg' (MkExpressionWitness witt exprr) =
            MkExpressionWitness <$> mapNeg witt <*> mapWitnessesM mapPos mapNeg exprr
        in mapWitnessesM @Type @poswit @(ExpressionWitness negwit (NamedExpression name negwit)) mapPos mapNeg' spat
