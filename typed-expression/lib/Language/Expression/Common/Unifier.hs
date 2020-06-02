module Language.Expression.Common.Unifier where

import Data.Shim
import Language.Expression.Common.Named
import Language.Expression.Common.Sealed
import Language.Expression.Common.WitnessMappable
import Shapes

newtype UUShim (unifier :: Type -> Type) (a :: Type) (b :: Type) = MkUUShim
    { uuGetShim :: unifier (UnifierShim unifier a b)
    }

instance Unifier unifier => Category (UUShim unifier) where
    id = MkUUShim $ pure cid
    MkUUShim ubc . MkUUShim uab = MkUUShim $ liftA2 (<.>) ubc uab

instance Unifier unifier => InCategory (UUShim unifier) where
    cid = id
    (<.>) = (.)

instance Unifier unifier => JoinMeetCategory (UUShim unifier) where
    initf = MkUUShim $ pure initf
    termf = MkUUShim $ pure termf
    join1 = MkUUShim $ pure join1
    join2 = MkUUShim $ pure join2
    joinf (MkUUShim uar) (MkUUShim ubr) = MkUUShim $ liftA2 joinf uar ubr
    meet1 = MkUUShim $ pure meet1
    meet2 = MkUUShim $ pure meet2
    meetf (MkUUShim uar) (MkUUShim ubr) = MkUUShim $ liftA2 meetf uar ubr
    applf (MkUUShim uar) (MkUUShim ubr) = MkUUShim $ liftA2 applf uar ubr

uuLiftShim :: Unifier unifier => UnifierShim unifier a b -> UUShim unifier a b
uuLiftShim conv = MkUUShim $ pure conv

type UUNegShimWit unifier = ShimWit (UUShim unifier) (UnifierNegWitness unifier) 'Negative

type UUPosShimWit unifier = ShimWit (UUShim unifier) (UnifierPosWitness unifier) 'Positive

uuLiftNegShimWit :: Unifier unifier => UnifierNegShimWit unifier t -> UUNegShimWit unifier t
uuLiftNegShimWit t = unNegShimWit t $ \wt conv -> mkNegShimWit wt $ uuLiftShim conv

uuLiftPosShimWit :: Unifier unifier => UnifierPosShimWit unifier t -> UUPosShimWit unifier t
uuLiftPosShimWit t = unPosShimWit t $ \wt conv -> mkPosShimWit wt $ uuLiftShim conv

uuGetNegShimWit :: Unifier unifier => UUNegShimWit unifier t -> unifier (UnifierNegShimWit unifier t)
uuGetNegShimWit t = unNegShimWit t $ \wt (MkUUShim uconv) -> fmap (\conv -> mkNegShimWit wt conv) uconv

uuGetPosShimWit :: Unifier unifier => UUPosShimWit unifier t -> unifier (UnifierPosShimWit unifier t)
uuGetPosShimWit t = unPosShimWit t $ \wt (MkUUShim uconv) -> fmap (\conv -> mkPosShimWit wt conv) uconv

class (Monad (UnifierMonad unifier), Applicative unifier, Eq (UnifierName unifier), Shim (UnifierShim unifier)) =>
          Unifier (unifier :: Type -> Type) where
    type UnifierName unifier :: Type
    type UnifierMonad unifier :: Type -> Type
    type UnifierNegWitness unifier :: Type -> Type
    type UnifierPosWitness unifier :: Type -> Type
    type UnifierSubstitutions unifier :: Type
    type UnifierShim unifier :: Type -> Type -> Type
    unifyNegWitnesses ::
           UnifierNegWitness unifier a
        -> UnifierNegWitness unifier b
        -> UnifierMonad unifier (UUNegShimWit unifier (MeetType a b))
    unifyPosWitnesses ::
           UnifierPosWitness unifier a
        -> UnifierPosWitness unifier b
        -> UnifierMonad unifier (UUPosShimWit unifier (JoinType a b))
    unifyPosNegWitnesses ::
           UnifierPosWitness unifier a -> UnifierNegWitness unifier b -> UnifierMonad unifier (UUShim unifier a b)
    solveUnifier :: unifier a -> UnifierMonad unifier (a, UnifierSubstitutions unifier)
    unifierPosSubstitute ::
           UnifierSubstitutions unifier
        -> UnifierPosWitness unifier t
        -> UnifierMonad unifier (UnifierPosShimWit unifier t)
    unifierNegSubstitute ::
           UnifierSubstitutions unifier
        -> UnifierNegWitness unifier t
        -> UnifierMonad unifier (UnifierNegShimWit unifier t)
    simplify ::
           forall a. UnifierMappable unifier a
        => a
        -> UnifierMonad unifier a

unifyUUNegShimWit ::
       forall unifier a b. Unifier unifier
    => UUNegShimWit unifier a
    -> UUNegShimWit unifier b
    -> UnifierMonad unifier (UUNegShimWit unifier (MeetType a b))
unifyUUNegShimWit (MkShimWit wa conva) (MkShimWit wb convb) = do
    uab <- unifyNegWitnesses @unifier wa wb
    return $ mapShimWit (polarBimap conva convb) uab

unifyUUPosShimWit ::
       forall unifier a b. Unifier unifier
    => UUPosShimWit unifier a
    -> UUPosShimWit unifier b
    -> UnifierMonad unifier (UUPosShimWit unifier (JoinType a b))
unifyUUPosShimWit (MkShimWit wa conva) (MkShimWit wb convb) = do
    uab <- unifyPosWitnesses @unifier wa wb
    return $ mapShimWit (polarBimap conva convb) uab

unifyUUPosNegShimWit ::
       forall unifier a b. Unifier unifier
    => UUPosShimWit unifier a
    -> UUNegShimWit unifier b
    -> UnifierMonad unifier (UUShim unifier a b)
unifyUUPosNegShimWit ta tb =
    unPosShimWit ta $ \wa conva ->
        unNegShimWit tb $ \wb convb -> do
            uab <- unifyPosNegWitnesses @unifier wa wb
            return $ convb . uab . conva

type UnifierNegShimWit unifier = ShimWit (UnifierShim unifier) (UnifierNegWitness unifier) 'Negative

type UnifierPosShimWit unifier = ShimWit (UnifierShim unifier) (UnifierPosWitness unifier) 'Positive

type UnifierMappable unifier = WitnessMappable (UnifierPosShimWit unifier) (UnifierNegShimWit unifier)

type UnifierOpenExpression unifier = NamedExpression (UnifierName unifier) (UnifierNegShimWit unifier)

type UnifierSealedExpression unifier
     = SealedExpression (UnifierName unifier) (UnifierNegShimWit unifier) (UnifierPosShimWit unifier)

type UnifierOpenPattern unifier = NamedPattern (UnifierName unifier) (UnifierPosShimWit unifier)

type UnifierSealedPattern unifier
     = SealedPattern (UnifierName unifier) (UnifierPosShimWit unifier) (UnifierNegShimWit unifier)

type UnifierPatternConstructor unifier
     = PatternConstructor (UnifierName unifier) (UnifierPosShimWit unifier) (UnifierNegShimWit unifier)

liftUnifier :: Monad (UnifierMonad unifier) => unifier a -> Compose (UnifierMonad unifier) unifier a
liftUnifier ua = Compose $ return ua

solveUnifyPosNegShimWit ::
       forall unifier a b. Unifier unifier
    => UnifierPosShimWit unifier a
    -> UnifierNegShimWit unifier b
    -> UnifierMonad unifier (UnifierShim unifier a b)
solveUnifyPosNegShimWit wa wb = do
    MkUUShim uab <- unifyUUPosNegShimWit @unifier (uuLiftPosShimWit wa) (uuLiftNegShimWit wb)
    (ab, _) <- solveUnifier uab
    return ab

unifierSubstitute ::
       forall unifier a. (Unifier unifier, UnifierMappable unifier a)
    => UnifierSubstitutions unifier
    -> a
    -> UnifierMonad unifier a
unifierSubstitute subs =
    mapWitnessesM
        (chainShimWitM $ unifierPosSubstitute @unifier subs)
        (chainShimWitM $ unifierNegSubstitute @unifier subs)

unifierSubstituteAndSimplify ::
       forall unifier a. (Unifier unifier, UnifierMappable unifier a)
    => UnifierSubstitutions unifier
    -> a
    -> UnifierMonad unifier a
unifierSubstituteAndSimplify subs a = do
    a' <- unifierSubstitute @unifier subs a
    simplify @unifier a'

unifierSolve ::
       forall unifier a. (Unifier unifier, UnifierMappable unifier a)
    => unifier a
    -> UnifierMonad unifier a
unifierSolve ua = do
    (a, subs) <- solveUnifier @unifier ua
    unifierSubstituteAndSimplify @unifier subs a
