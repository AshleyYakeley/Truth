module Language.Expression.Common.Unifier where

import Data.Shim
import Language.Expression.Common.Simplifier
import Language.Expression.Common.TypeSystem
import Language.Expression.Common.WitnessMappable
import Shapes

newtype UUShim (ts :: Type) (a :: Type) (b :: Type) = MkUUShim
    { uuGetShim :: Unifier ts (TSShim ts a b)
    }

instance UnifyTypeSystem ts => Category (UUShim ts) where
    id = MkUUShim $ pure cid
    MkUUShim ubc . MkUUShim uab = MkUUShim $ liftA2 (<.>) ubc uab

instance UnifyTypeSystem ts => InCategory (UUShim ts) where
    cid = id
    (<.>) = (.)

instance UnifyTypeSystem ts => JoinMeetIsoCategory (UUShim ts)

instance UnifyTypeSystem ts => JoinMeetCategory (UUShim ts) where
    initf = MkUUShim $ pure initf
    termf = MkUUShim $ pure termf
    join1 = MkUUShim $ pure join1
    join2 = MkUUShim $ pure join2
    joinf (MkUUShim uar) (MkUUShim ubr) = MkUUShim $ liftA2 joinf uar ubr
    meet1 = MkUUShim $ pure meet1
    meet2 = MkUUShim $ pure meet2
    meetf (MkUUShim uar) (MkUUShim ubr) = MkUUShim $ liftA2 meetf uar ubr
    applf (MkUUShim uar) (MkUUShim ubr) = MkUUShim $ liftA2 applf uar ubr

uuLiftShim :: UnifyTypeSystem ts => TSShim ts a b -> UUShim ts a b
uuLiftShim conv = MkUUShim $ pure conv

type UUNegShimWit ts = ShimWit (UUShim ts) (TSNegWitness ts) 'Negative

type UUPosShimWit ts = ShimWit (UUShim ts) (TSPosWitness ts) 'Positive

uuLiftNegShimWit :: UnifyTypeSystem ts => TSNegShimWit ts t -> UUNegShimWit ts t
uuLiftNegShimWit t = unNegShimWit t $ \wt conv -> mkNegShimWit wt $ uuLiftShim conv

uuLiftPosShimWit :: UnifyTypeSystem ts => TSPosShimWit ts t -> UUPosShimWit ts t
uuLiftPosShimWit t = unPosShimWit t $ \wt conv -> mkPosShimWit wt $ uuLiftShim conv

uuGetNegShimWit :: UnifyTypeSystem ts => UUNegShimWit ts t -> Unifier ts (TSNegShimWit ts t)
uuGetNegShimWit t = unNegShimWit t $ \wt (MkUUShim uconv) -> fmap (\conv -> mkNegShimWit wt conv) uconv

uuGetPosShimWit :: UnifyTypeSystem ts => UUPosShimWit ts t -> Unifier ts (TSPosShimWit ts t)
uuGetPosShimWit t = unPosShimWit t $ \wt (MkUUShim uconv) -> fmap (\conv -> mkPosShimWit wt conv) uconv

class (TypeSystem ts, Applicative (Unifier ts), CartesianShim (TSShim ts), Show (UnifierSubstitutions ts)) =>
          UnifyTypeSystem (ts :: Type) where
    type Unifier ts :: Type -> Type
    type UnifierSubstitutions ts :: Type
    unifyNegWitnesses :: TSNegWitness ts a -> TSNegWitness ts b -> TSOuter ts (UUNegShimWit ts (MeetType a b))
    unifyPosWitnesses :: TSPosWitness ts a -> TSPosWitness ts b -> TSOuter ts (UUPosShimWit ts (JoinType a b))
    unifyPosNegWitnesses :: TSPosWitness ts a -> TSNegWitness ts b -> TSOuter ts (UUShim ts a b)
    solveUnifier :: Unifier ts a -> TSOuter ts (a, UnifierSubstitutions ts)
    unifierPosSubstitute :: UnifierSubstitutions ts -> TSPosWitness ts t -> TSOuter ts (TSPosShimWit ts t)
    unifierNegSubstitute :: UnifierSubstitutions ts -> TSNegWitness ts t -> TSOuter ts (TSNegShimWit ts t)

unifyUUNegShimWit ::
       forall ts a b. UnifyTypeSystem ts
    => UUNegShimWit ts a
    -> UUNegShimWit ts b
    -> TSOuter ts (UUNegShimWit ts (MeetType a b))
unifyUUNegShimWit (MkShimWit wa conva) (MkShimWit wb convb) = do
    uab <- unifyNegWitnesses @ts wa wb
    return $ mapShimWit (iPolarPair conva convb) uab

unifyUUPosShimWit ::
       forall ts a b. UnifyTypeSystem ts
    => UUPosShimWit ts a
    -> UUPosShimWit ts b
    -> TSOuter ts (UUPosShimWit ts (JoinType a b))
unifyUUPosShimWit (MkShimWit wa conva) (MkShimWit wb convb) = do
    uab <- unifyPosWitnesses @ts wa wb
    return $ mapShimWit (iPolarPair conva convb) uab

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

solveUnifyPosNegShimWit ::
       forall ts a b. UnifyTypeSystem ts
    => TSPosShimWit ts a
    -> TSNegShimWit ts b
    -> TSOuter ts (TSShim ts a b)
solveUnifyPosNegShimWit wa wb = do
    MkUUShim uab <- unifyUUPosNegShimWit @ts (uuLiftPosShimWit wa) (uuLiftNegShimWit wb)
    (ab, _) <- solveUnifier @ts uab
    return ab

unifierSubstitute ::
       forall ts a. (UnifyTypeSystem ts, TSMappable ts a)
    => UnifierSubstitutions ts
    -> a
    -> TSOuter ts a
unifierSubstitute subs =
    mapWitnessesM (chainShimWitM $ unifierPosSubstitute @ts subs) (chainShimWitM $ unifierNegSubstitute @ts subs)

unifierSubstituteAndSimplify ::
       forall ts a. (UnifyTypeSystem ts, SimplifyTypeSystem ts, TSMappable ts a)
    => UnifierSubstitutions ts
    -> a
    -> TSOuter ts a
unifierSubstituteAndSimplify subs a = do
    a' <- unifierSubstitute @ts subs a
    simplify @ts a'

unifierSolve ::
       forall ts a. (UnifyTypeSystem ts, SimplifyTypeSystem ts, TSMappable ts a)
    => Unifier ts a
    -> TSOuter ts a
unifierSolve ua = do
    (a, subs) <- solveUnifier @ts ua
    unifierSubstituteAndSimplify @ts subs a
