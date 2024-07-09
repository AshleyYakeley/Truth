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

type NamedExpressionWitness name tw = ExpressionWitness tw (NamedExpression name tw)

type SealedExpressionPattern (name :: Type) (vw :: Type -> Type) (tw :: Type -> Type)
     = SealedNamedPattern name vw (NamedExpressionWitness name tw)

liftExpressionShimWit ::
       (Applicative expr, JoinMeetShim shim) => ShimWit shim wit t -> ShimWit shim (ExpressionWitness wit expr) t
liftExpressionShimWit (MkShimWit wtt conv) = MkShimWit (MkExpressionWitness wtt $ pure MkTopType) $ meetf conv termf

varSealedExpressionPattern :: name -> tw t -> vw (MeetType t ()) -> SealedExpressionPattern name vw tw
varSealedExpressionPattern n twt vwt = varSealedNamedPattern n (MkExpressionWitness twt $ pure ()) vwt

anySealedExpressionPattern :: tw t -> SealedExpressionPattern name vw tw
anySealedExpressionPattern twt = anySealedPattern $ MkExpressionWitness twt $ pure ()

type ExpressionPatternConstructor (name :: Type) (vw :: Type -> Type) (tw :: Type -> Type)
     = PatternConstructor (NameWitness name vw) vw (NamedExpressionWitness name tw)

toExpressionPatternConstructor ::
       PatternConstructor (NameWitness name vw) vw tw -> ExpressionPatternConstructor name vw tw
toExpressionPatternConstructor (MkPatternConstructor twt lt pat) =
    MkPatternConstructor (MkExpressionWitness twt $ pure MkTopType) lt $ pat . arr meet1
