-- | the "U" is for "unsafe". This entire module is built on lies.
module Language.Expression.Common.TypeVariable
    ( UVar
    , UVarT
    , uVarName
    , assignUVarT
    , TypeVar(..)
    , TypeVarT
    , typeVarName
    , newTypeVar
    , assignTypeVarT
    , assignSameTypeVarT
    , assignTypeVarWit
    , newAssignTypeVar
    , SomeTypeVarT(..)
    , someTypeVarName
    ) where

import Shapes
import Shapes.Unsafe (unsafeRefl)

type UVar :: forall (k :: Type) -> Symbol -> k
type family UVar k name

type UVarT :: Symbol -> Type
type UVarT name = UVar Type name

uVarName :: forall (name :: Symbol). SymbolType name -> String
uVarName = witnessToValue

newUVar :: forall r. String -> (forall (newname :: Symbol). SymbolType newname -> r) -> r
newUVar = valueToWitness

-- | The key to using this safely is not to assign the same name more than once.
-- If you're seeing what looks like bad unsafe coercions, this is probably the problem.
assignUVar :: forall (k :: Type) (t :: k) (name :: Symbol) r. SymbolType name -> (UVar k name ~ t => r) -> r
assignUVar _ = withRefl $ unsafeRefl @k @(UVar k name) @t

assignUVarT :: forall (t :: Type) (name :: Symbol) r. SymbolType name -> (UVarT name ~ t => r) -> r
assignUVarT = assignUVar @Type @t

assignUVarWit ::
       forall (k :: Type) (t :: k) (name :: Symbol) (w :: k -> Type) r.
       SymbolType name
    -> w t
    -> (UVar k name ~ t => r)
    -> r
assignUVarWit name _ = assignUVar @k @t name

type TypeVar :: forall k. k -> Type
data TypeVar tv where
    MkTypeVar :: forall k (name :: Symbol). SymbolType name -> TypeVar (UVar k name)

type TypeVarT :: Type -> Type
type TypeVarT = TypeVar

instance Show (TypeVar tv) where
    show (MkTypeVar t) = show t

instance TestEquality TypeVar where
    testEquality (MkTypeVar a) (MkTypeVar b) = do
        Refl <- testEquality a b
        return Refl

typeVarName :: TypeVar tv -> String
typeVarName (MkTypeVar vsym) = uVarName vsym

newTypeVar :: forall r. String -> (forall tv. TypeVar tv -> r) -> r
newTypeVar name call = newUVar name $ \vsym -> call $ MkTypeVar vsym

assignTypeVarT :: forall (ta :: Type) tv r. TypeVarT tv -> (tv ~ ta => r) -> r
assignTypeVarT (MkTypeVar vsym) call = assignUVarT @ta vsym call

assignSameTypeVarT :: forall (ta :: Type) tv r. TypeVarT ta -> TypeVarT tv -> (tv ~ ta => r) -> r
assignSameTypeVarT _ = assignTypeVarT

assignTypeVarWit :: forall (k :: Type) (t :: k) (tv :: k) (w :: k -> Type) r. TypeVar tv -> w t -> (tv ~ t => r) -> r
assignTypeVarWit (MkTypeVar vsym) w call = assignUVarWit vsym w call

newAssignTypeVar :: forall (k :: Type) (tv :: k). String -> TypeVar tv
newAssignTypeVar nstr = newUVar nstr $ \vsym -> assignUVar @k @tv vsym $ MkTypeVar vsym

data SomeTypeVarT =
    forall tv. MkSomeTypeVarT (TypeVarT tv)

instance Eq SomeTypeVarT where
    MkSomeTypeVarT a == MkSomeTypeVarT b = isJust $ testEquality a b

instance Show SomeTypeVarT where
    show (MkSomeTypeVarT v) = show v

someTypeVarName :: SomeTypeVarT -> String
someTypeVarName (MkSomeTypeVarT v) = typeVarName v
