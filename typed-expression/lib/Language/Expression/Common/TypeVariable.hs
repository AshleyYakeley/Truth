module Language.Expression.Common.TypeVariable
    ( UVar
    , uVarName
    , newUVar
    , assignUVar
    , assignUVarWit
    , VarType(..)
    , varTypeName
    , newAssignUVar
    , renameUVar
    ) where

import Shapes
import Shapes.Unsafe (unsafeRefl)

type family UVar (k :: Type) (name :: Symbol) :: k where

uVarName :: forall (name :: Symbol). SymbolType name -> String
uVarName = witnessToValue

newUVar :: forall r. String -> (forall (newname :: Symbol). SymbolType newname -> r) -> r
newUVar = valueToWitness

assignUVar :: forall (k :: Type) (t :: k) (name :: Symbol) r. SymbolType name -> (UVar k name ~ t => r) -> r
assignUVar _ call =
    case unsafeRefl @k @(UVar k name) @t of
        Refl -> call

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

renameUVar :: forall (k :: Type) (oldname :: Symbol). SymbolType oldname -> String -> VarType (UVar k oldname)
renameUVar _ newname = newAssignUVar @k @(UVar k oldname) newname
