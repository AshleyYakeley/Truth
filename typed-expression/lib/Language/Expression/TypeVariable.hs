module Language.Expression.TypeVariable
    ( UVar
    , uVarName
    , newUVar
    , assignUVar
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

newAssignUVar ::
       forall (k :: Type) (t :: k) r.
       String
    -> (forall (newname :: Symbol). UVar k newname ~ t => SymbolType newname -> r)
    -> r
newAssignUVar nstr call = newUVar nstr $ \nsym -> assignUVar @k @t nsym $ call nsym

renameUVar ::
       forall (k :: Type) (oldname :: Symbol) r.
       String
    -> SymbolType oldname
    -> (forall (newname :: Symbol). UVar k newname ~ UVar k oldname => SymbolType newname -> r)
    -> r
renameUVar newname _ call = newAssignUVar @k @(UVar k oldname) newname call
