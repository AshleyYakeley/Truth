module Language.Expression.Typed where

import Language.Expression.Bindings
import Language.Expression.NameWit
import Language.Expression.Named
import Language.Expression.Sealed
import Shapes

class Monad (TSMonad ts) => TypeSystem (ts :: Type) where
    type VarWitness ts :: Type -> Type
    type ValWitness ts :: Type -> Type
    type TSMonad ts :: Type -> Type
    typeSystemVarJoiner :: TypeJoiner (TSMonad ts) (VarWitness ts)
    typeSystemChecker :: TypeChecker (TSMonad ts) (ValWitness ts) (VarWitness ts)
    typeSystemApplyWitness :: ApplyWitness (TSMonad ts) (ValWitness ts)
    typeSystemAbstractWitness :: AbstractWitness (TSMonad ts) (VarWitness ts) (ValWitness ts)
    typeSystemGenerateVariable :: (forall t. VarWitness ts t -> ValWitness ts t -> TSMonad ts r) -> TSMonad ts r

type TypedExpression name ts = SealedNamedExpression name (VarWitness ts) (ValWitness ts)

evalTypedExpression ::
       forall ts name m. (MonadFail m, Show name)
    => TypedExpression name ts
    -> m (Any (ValWitness ts))
evalTypedExpression = evalSealedExpression

applyTypedExpression ::
       forall ts name. TypeSystem ts
    => TypedExpression name ts
    -> TypedExpression name ts
    -> TSMonad ts (TypedExpression name ts)
applyTypedExpression = applySealedExpression (typeSystemApplyWitness @ts)

abstractTypedExpression ::
       forall ts name. (Eq name, TypeSystem ts)
    => name
    -> TypedExpression name ts
    -> TSMonad ts (TypedExpression name ts)
abstractTypedExpression = abstractSealedNamedExpression (typeSystemVarJoiner @ts) (typeSystemAbstractWitness @ts)

varTypedExpression ::
       forall ts name. TypeSystem ts
    => name
    -> TSMonad ts (TypedExpression name ts)
varTypedExpression name = typeSystemGenerateVariable @ts $ \vwt twt -> return $ varSealedNameExpression name vwt twt

constTypedExpression :: forall ts name t. ValWitness ts t -> t -> TypedExpression name ts
constTypedExpression = constSealedExpression

letTypedExpression ::
       forall ts name. (Eq name, TypeSystem ts)
    => name
    -> TypedExpression name ts
    -> TypedExpression name ts
    -> TSMonad ts (TypedExpression name ts)
letTypedExpression = letSealedNamedExpression (typeSystemVarJoiner @ts) (typeSystemChecker @ts)

type TypedBindings name ts = NamedBindings name (VarWitness ts) (ValWitness ts)

singleTypedBinding :: forall ts name. name -> TypedExpression name ts -> TypedBindings name ts
singleTypedBinding = singleNamedBinding

uncheckedBindingsLetTypedExpression ::
       forall ts name. (Eq name, TypeSystem ts)
    => TypedBindings name ts
    -> TypedExpression name ts
    -> TSMonad ts (TypedExpression name ts)
uncheckedBindingsLetTypedExpression =
    uncheckedBindingsLetSealedNamedExpression (typeSystemVarJoiner @ts) (typeSystemChecker @ts)

typedBindingsCheckDuplicates ::
       forall ts name m. (Eq name, Show name, TypeSystem ts, MonadFail m)
    => TypedBindings name ts
    -> m ()
typedBindingsCheckDuplicates = bindingsCheckDuplicates
