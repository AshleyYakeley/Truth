module Language.Expression.Typed where

import Language.Expression.Abstract
import Language.Expression.Bindings
import Language.Expression.Renamer
import Language.Expression.Sealed
import Language.Expression.Unifier
import Shapes

class ( Monad (TypeCheck ts)
      , Renamer (TypeRenamer ts)
      , RenamerNegWitness (TypeRenamer ts) ~ NegWitness ts
      , RenamerPosWitness (TypeRenamer ts) ~ PosWitness ts
      , Unifier (TypeUnifier ts)
      , UnifierMonad (TypeUnifier ts) ~ TypeRenamer ts (TypeCheck ts)
      , UnifierNegWitness (TypeUnifier ts) ~ NegWitness ts
      , UnifierPosWitness (TypeUnifier ts) ~ PosWitness ts
      ) => TypeSystem (ts :: Type) where
    type TypeRenamer ts :: (Type -> Type) -> (Type -> Type)
    type TypeUnifier ts :: Type -> Type
    type NegWitness ts :: Type -> Type
    type PosWitness ts :: Type -> Type
    type TypeCheck ts :: Type -> Type
    typeSystemFunctionPosWitness :: FunctionPosWitness (NegWitness ts) (PosWitness ts)
    typeSystemFunctionNegWitness :: FunctionNegWitness (NegWitness ts) (PosWitness ts)

type TypedExpression name ts = SealedExpression name (NegWitness ts) (PosWitness ts)

type TypeMonadRenamer ts = TypeRenamer ts (TypeCheck ts)

typedUnifyPosNegWitnesses ::
       forall ts a b. TypeSystem ts
    => PosWitness ts a
    -> NegWitness ts b
    -> TypeCheck ts (a -> b)
typedUnifyPosNegWitnesses wa wb = runRenamer @(TypeRenamer ts) $ solveUnifyPosNegWitnesses @(TypeUnifier ts) wa wb

evalTypedExpression ::
       forall ts name m. (MonadFail m, Show name)
    => TypedExpression name ts
    -> m (Any (PosWitness ts))
evalTypedExpression = evalSealedExpression

typedAnyToVal ::
       forall ts t. TypeSystem ts
    => NegWitness ts t
    -> Any (PosWitness ts)
    -> TypeCheck ts t
typedAnyToVal witn (MkAny witp val) = do
    conv <- typedUnifyPosNegWitnesses @ts witp witn
    return $ conv val

evalTypedExpressionToType ::
       forall ts name t. (TypeSystem ts, MonadFail (TypeCheck ts), Show name)
    => NegWitness ts t
    -> TypedExpression name ts
    -> TypeCheck ts t
evalTypedExpressionToType witn expr = do
    aval <- evalTypedExpression @ts expr
    typedAnyToVal @ts witn aval

applyTypedExpression ::
       forall ts name. TypeSystem ts
    => TypedExpression name ts
    -> TypedExpression name ts
    -> TypeCheck ts (TypedExpression name ts)
applyTypedExpression tf ta =
    runRenamer @(TypeRenamer ts) $
    applySealedExpression @(TypeRenamer ts) @(TypeUnifier ts) (typeSystemFunctionNegWitness @ts) tf ta

abstractTypedExpression ::
       forall ts name. (Eq name, TypeSystem ts)
    => name
    -> TypedExpression name ts
    -> TypeCheck ts (TypedExpression name ts)
abstractTypedExpression n expr =
    runRenamer @(TypeRenamer ts) $
    abstractSealedExpression @(TypeRenamer ts) @(TypeUnifier ts) (typeSystemFunctionPosWitness @ts) n expr

varTypedExpression ::
       forall ts name. TypeSystem ts
    => name
    -> TypedExpression name ts
varTypedExpression name =
    runIdentity $
    runRenamer @(TypeRenamer ts) $
    renameNewVar $ \vwt twt conv -> withTransConstraintTM @Monad $ return $ varSealedExpression name vwt twt conv

constTypedExpression :: forall ts name. Any (PosWitness ts) -> TypedExpression name ts
constTypedExpression = constSealedExpression

letTypedExpression ::
       forall ts name. (Eq name, TypeSystem ts)
    => name
    -> TypedExpression name ts
    -> TypedExpression name ts
    -> TypeCheck ts (TypedExpression name ts)
letTypedExpression n expv expb =
    runRenamer @(TypeRenamer ts) $ letSealedExpression @(TypeRenamer ts) @(TypeUnifier ts) n expv expb

type TypedBindings name ts = Bindings name (TypeUnifier ts)

singleTypedBinding ::
       forall ts name. TypeSystem ts
    => name
    -> TypedExpression name ts
    -> TypedBindings name ts
singleTypedBinding = singleBinding

uncheckedBindingsLetTypedExpression ::
       forall ts name. (Eq name, TypeSystem ts)
    => TypedBindings name ts
    -> TypedExpression name ts
    -> TypeCheck ts (TypedExpression name ts)
uncheckedBindingsLetTypedExpression bb expb =
    runRenamer @(TypeRenamer ts) $ bindingsLetSealedExpression @(TypeRenamer ts) @(TypeUnifier ts) bb expb

typedBindingsCheckDuplicates ::
       forall ts name m. (Eq name, Show name, TypeSystem ts, MonadFail m)
    => TypedBindings name ts
    -> m ()
typedBindingsCheckDuplicates = bindingsCheckDuplicates
