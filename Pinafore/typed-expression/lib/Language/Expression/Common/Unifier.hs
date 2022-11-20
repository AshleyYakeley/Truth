module Language.Expression.Common.Unifier where

import Data.Shim
import Language.Expression.Common.Pattern
import Language.Expression.Common.Simplifier
import Language.Expression.Common.SolverExpression
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.WitnessMappable
import Shapes

type UUShim (ts :: Type) = ComposeShim (UnifierExpression ts) (TSShim ts)

uuLiftShim :: UnifyTypeSystem ts => TSShim ts a b -> UUShim ts a b
uuLiftShim = pureComposeShim

uuGetShim :: forall ts a b. UUShim ts a b -> UnifierExpression ts (TSShim ts a b)
uuGetShim = unComposeShim

type UUNegShimWit ts = PolarShimWit (UUShim ts) (TSNegWitness ts) 'Negative

type UUPosShimWit ts = PolarShimWit (UUShim ts) (TSPosWitness ts) 'Positive

uuLiftNegShimWit ::
       forall ts t. UnifyTypeSystem ts
    => TSNegShimWit ts t
    -> UUNegShimWit ts t
uuLiftNegShimWit t = unNegShimWit t $ \wt conv -> mkNegShimWit wt $ uuLiftShim @ts conv

uuLiftPosShimWit ::
       forall ts t. UnifyTypeSystem ts
    => TSPosShimWit ts t
    -> UUPosShimWit ts t
uuLiftPosShimWit t = unPosShimWit t $ \wt conv -> mkPosShimWit wt $ uuLiftShim @ts conv

uuLiftNegExpressionShimWit ::
       forall ts t. UnifyTypeSystem ts
    => TSExpressionWitness ts t
    -> UUNegShimWit ts t
uuLiftNegExpressionShimWit (MkExpressionWitness (MkNegShimWit tt conv) expr) =
    MkNegShimWit tt $
    MkComposeShim $ solverExpressionLiftValue $ fmap (\r -> functionToShim "ttr" (\t -> MkMeetType (t, r)) . conv) expr

class (TypeSystem ts, Applicative (Unifier ts), CartesianShim (TSShim ts), Show (UnifierSubstitutions ts)) =>
          UnifyTypeSystem (ts :: Type) where
    type Unifier ts :: Type -> Type
    type UnifierSubstitutions ts :: Type
    unifyNegWitnesses :: TSNegWitness ts a -> TSNegWitness ts b -> TSOuter ts (UUNegShimWit ts (MeetType a b))
    unifyPosWitnesses :: TSPosWitness ts a -> TSPosWitness ts b -> TSOuter ts (UUPosShimWit ts (JoinType a b))
    unifyPosNegWitnesses :: TSPosWitness ts a -> TSNegWitness ts b -> TSOuter ts (UUShim ts a b)
    solveUnifier :: Unifier ts a -> TSOuter ts (TSOpenExpression ts a, UnifierSubstitutions ts)
    unifierPosSubstitute :: UnifierSubstitutions ts -> TSPosWitness ts t -> TSOuter ts (TSPosShimWit ts t)
    unifierNegSubstitute :: UnifierSubstitutions ts -> TSNegWitness ts t -> TSOuter ts (TSNegShimWit ts t)

type UnifierExpression ts = TSOpenSolverExpression ts (Unifier ts)

unifyUUNegShimWit ::
       forall ts a b. UnifyTypeSystem ts
    => UUNegShimWit ts a
    -> UUNegShimWit ts b
    -> TSOuter ts (UUNegShimWit ts (MeetType a b))
unifyUUNegShimWit (MkShimWit wa conva) (MkShimWit wb convb) = do
    uab <- unifyNegWitnesses @ts wa wb
    return $ mapPolarShimWit (iPolarPair conva convb) uab

unifyUUPosShimWit ::
       forall ts a b. UnifyTypeSystem ts
    => UUPosShimWit ts a
    -> UUPosShimWit ts b
    -> TSOuter ts (UUPosShimWit ts (JoinType a b))
unifyUUPosShimWit (MkShimWit wa conva) (MkShimWit wb convb) = do
    uab <- unifyPosWitnesses @ts wa wb
    return $ mapPolarShimWit (iPolarPair conva convb) uab

unifyUUPosNegShimWit ::
       forall ts a b. UnifyTypeSystem ts
    => UUPosShimWit ts a
    -> UUNegShimWit ts b
    -> TSOuter ts (UUShim ts a b)
unifyUUPosNegShimWit ta tb =
    unPosShimWit ta $ \wa conva ->
        unNegShimWit tb $ \wb convb -> do
            uab <- unifyPosNegWitnesses @ts wa wb
            return $ convb . uab . conva

unifyPosNegShimWit ::
       forall ts a b. UnifyTypeSystem ts
    => TSPosShimWit ts a
    -> TSNegShimWit ts b
    -> TSOuter ts (TSOpenExpression ts (TSShim ts a b), UnifierSubstitutions ts)
unifyPosNegShimWit wa wb = do
    MkComposeShim uexpr <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit @ts wa) (uuLiftNegShimWit @ts wb)
    solveUnifierExpression @ts uexpr

solveUnifierExpression ::
       forall ts a. UnifyTypeSystem ts
    => UnifierExpression ts a
    -> TSOuter ts (TSOpenExpression ts a, UnifierSubstitutions ts)
solveUnifierExpression (MkSolverExpression ut eta) = do
    (texpr, subs) <- solveUnifier @ts ut
    return $ (eta <*> texpr, subs)

solveUUShim ::
       forall ts a b. UnifyTypeSystem ts
    => UUShim ts a b
    -> TSOuter ts (TSOpenExpression ts (TSShim ts a b), UnifierSubstitutions ts)
solveUUShim (MkComposeShim uuconv) = solveUnifierExpression @ts uuconv

unifierSubstitute ::
       forall ts a. (UnifyTypeSystem ts, TSMappable ts a)
    => UnifierSubstitutions ts
    -> a
    -> TSOuter ts a
unifierSubstitute subs =
    mapWitnessesM
        (chainPolarShimWitM $ unifierPosSubstitute @ts subs)
        (chainPolarShimWitM $ unifierNegSubstitute @ts subs)

unifierSubstituteAndSimplify ::
       forall ts a. (UnifyTypeSystem ts, SimplifyTypeSystem ts, TSMappable ts a)
    => UnifierSubstitutions ts
    -> a
    -> TSOuter ts a
unifierSubstituteAndSimplify subs a = do
    a' <- unifierSubstitute @ts subs a
    simplify @ts a'
