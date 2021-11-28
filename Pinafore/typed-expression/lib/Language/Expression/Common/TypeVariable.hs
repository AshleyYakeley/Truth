-- | the "U" is for "unsafe". This entire module is built on lies.
module Language.Expression.Common.TypeVariable
    ( UVar
    , UVarT
    , uVarName
    , newUVar
    , AnyVar(..)
    , newUVarAny
    , assignUVar
    , assignUVarT
    , assignUVarWit
    , VarType(..)
    , varTypeName
    , newAssignUVar
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

data AnyVar =
    forall name. MkAnyVar (SymbolType name)

newUVarAny :: String -> AnyVar
newUVarAny nstr = newUVar nstr MkAnyVar

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

type VarType :: forall k. k -> Type
data VarType t where
    MkVarType :: forall k (name :: Symbol). SymbolType name -> VarType (UVar k name)

varTypeName :: VarType t -> String
varTypeName (MkVarType var) = uVarName var

newAssignUVar :: forall (k :: Type) (t :: k). String -> VarType t
newAssignUVar nstr = newUVar nstr $ \nsym -> assignUVar @k @t nsym $ MkVarType nsym
