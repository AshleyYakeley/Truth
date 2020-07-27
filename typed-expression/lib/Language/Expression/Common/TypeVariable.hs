-- | the "U" is for "unsafe". This entire module is built on lies.
module Language.Expression.Common.TypeVariable
    ( UVar
    , UVarT
    , uVarName
    , newUVar
    , assignUVar
    , assignUVarWit
    , VarType(..)
    , varTypeName
    , newAssignUVar
    , renameUVar
    , TF(..)
    , Apply
    , ApplyFunctor(..)
    , bijApplyFunctor
    , USub
    , usubIdentity
    , usubResub
    , usubConstant
    , assignUSub
    , renameUSub
    ) where

import Shapes
import Shapes.Unsafe (unsafeRefl)

type family UVar (k :: Type) (name :: Symbol) :: k where

type UVarT :: Symbol -> Type
type UVarT name = UVar Type name

uVarName :: forall (name :: Symbol). SymbolType name -> String
uVarName = witnessToValue

newUVar :: forall r. String -> (forall (newname :: Symbol). SymbolType newname -> r) -> r
newUVar = valueToWitness

assignUVar :: forall (k :: Type) (t :: k) (name :: Symbol) r. SymbolType name -> (UVar k name ~ t => r) -> r
assignUVar _ = withRefl $ unsafeRefl @k @(UVar k name) @t

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

data TF kp kq
    = TFConstant kq
    | TFConstructor (kp -> kq)
    | TFOther

type Apply :: TF kp kq -> kp -> kq
type family Apply s t where
    Apply ('TFConstant a) t = a
    Apply ('TFConstructor f) t = f t

type ApplyFunctor :: TF Type Type -> Type
newtype ApplyFunctor tf = MkApplyFunctor
    { unApplyFunctor :: forall a b. (a -> b) -> Apply tf a -> Apply tf b
    }

bijApplyFunctor ::
       forall (tf :: TF Type Type) (a :: Type) (b :: Type).
       ApplyFunctor tf
    -> Bijection a b
    -> Bijection (Apply tf a) (Apply tf b)
bijApplyFunctor (MkApplyFunctor afmap) (MkIsomorphism ab ba) = MkIsomorphism (afmap ab) (afmap ba)

type USub :: Symbol -> Type -> TF Type Type
type family USub name t where

usubConstant :: forall (t :: Type) (name :: Symbol). SymbolType name -> USub name t :~: 'TFConstant t
usubConstant _ = unsafeRefl

usubIdentity :: forall (t :: Type) (name :: Symbol). SymbolType name -> Apply (USub name t) (UVarT name) :~: t
usubIdentity _ = unsafeRefl

usubResub ::
       forall (oldname :: Symbol) (newname :: Symbol) w (t :: Type).
       SymbolType oldname
    -> SymbolType newname
    -> w t
    -> USub newname (Apply (USub oldname t) (UVarT newname)) :~: USub oldname t
usubResub _ _ _ = unsafeRefl

assignUSub :: forall (name :: Symbol) (t :: Type) (x :: Type) (t' :: Type). Apply (USub name t) x :~: t'
assignUSub = unsafeRefl

renameUSub ::
       forall (oldname :: Symbol) (newname :: Symbol) (t :: Type).
       SymbolType oldname
    -> SymbolType newname
    -> Apply (USub oldname t) (UVarT newname) :~: t
renameUSub _ _ = assignUSub @oldname @t @(UVarT newname) @t
