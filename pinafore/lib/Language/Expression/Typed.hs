module Language.Expression.Typed where

import Language.Expression.Expression
import Shapes

class FunctionWitness (valwit :: k -> Type) where
    -- abstractWitness :: forall a b r. wit a -> wit b -> (forall ab. wit ab -> ((a -> b) -> ab) -> r) -> Result Text r
    applyWitness ::
           forall (r :: Type) (ab :: k) (a :: k).
           valwit ab
        -> valwit a
        -> (forall (a' :: k) (b' :: k).
                    valwit b' -> KindMorphism k ab (KindFunction k a' b') -> KindMorphism k a a' -> r)
        -> Result Text r

type family TypeSystemKind (tsys :: Type) :: Type -- can't go in the class because reasons

class (FunctionKind (TypeSystemKind tsys), FunctionWitness (PositiveWitness tsys), UnifyWitness (NegativeWitness tsys)) =>
          TypeSystem (tsys :: Type) where
    type PositiveWitness tsys :: TypeSystemKind tsys -> Type
    type NegativeWitness tsys :: TypeSystemKind tsys -> Type

data TypedExpression tsys (f :: TypeSystemKind tsys -> Type) =
    forall (t :: TypeSystemKind tsys). MkTypedExpression (PositiveWitness tsys t)
                                                         (Expression (NegativeWitness tsys) f t)

tExprApply ::
       forall (tsys :: Type) (f :: TypeSystemKind tsys -> Type). KindApplicative f
    => TypeSystem tsys => TypedExpression tsys f -> TypedExpression tsys f -> Result Text (TypedExpression tsys f)
tExprApply (MkTypedExpression witf exprf) (MkTypedExpression wita expra) =
    applyWitness witf wita $ \witb convertf converta ->
        MkTypedExpression witb $ (kfmap convertf exprf) `kap` (kfmap converta expra)
--tExprAbstract :: Name -> TypedExpression tsys -> TypedExpression tsys
--tExprAbstract
{-
data Thing = MkThing (forall a. a -> a)

newtype Forall (f :: [Type] -> Type) = MkForall (forall (a :: [Type]). f a)

Expression tsym (forall a. a -> a)

forall a. Expression tsym (a -> a)
-}
