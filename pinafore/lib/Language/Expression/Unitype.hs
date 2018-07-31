{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Unitype
    ( SealedUnitypeExpression
    , abstractSealedUnitypeExpression
    , varSealedUnitypeExpression
    , evalSealedUnitypeExpression
    , UnitypeBindings
    , bindExpression
    , letSealedUnitypeExpression
    , bindingsLetExpression
    , uncheckedBindingsLetExpression
    ) where

import Data.List (head, tail)
import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Sealed
import Shapes

type UnitypeExpression name val = NamedExpression name ((:~:) val)

uniNamedMatch :: Eq name => name -> NamedWitness name ((:~:) t) t' -> Maybe (t -> t')
uniNamedMatch name (MkNamedWitness name' Refl) =
    if name == name'
        then Just id
        else Nothing

abstractUniNamedExpression :: Eq name => name -> UnitypeExpression name val a -> UnitypeExpression name val (val -> a)
abstractUniNamedExpression name = abstractExpression $ uniNamedMatch name

type SealedUnitypeExpression name val = SealedNamedExpression name ((:~:) val) ((:~:) val)

pattern MkSealedUnitypeExpression ::
        UnitypeExpression name val val -> SealedUnitypeExpression name val

pattern MkSealedUnitypeExpression expr =
        MkSealedExpression Refl expr

{-# COMPLETE MkSealedUnitypeExpression #-}

type instance Element (SealedUnitypeExpression name val) = val

instance MonoFunctor (SealedUnitypeExpression name val) where
    omap ab (MkSealedUnitypeExpression expr) = MkSealedUnitypeExpression $ fmap ab expr

instance MonoPointed (SealedUnitypeExpression name val) where
    opoint = constSealedExpression Refl

instance MonoApplicative (SealedUnitypeExpression name val) where
    oliftA2 appf vexpr bexpr =
        runIdentity $ applySealedExpression (\Refl Refl cont -> Identity $ cont Refl appf) vexpr bexpr
    osequenceA conv exprs = MkSealedUnitypeExpression $ fmap conv $ sequenceA $ fmap unSealedUnitypeExpression exprs

unSealedUnitypeExpression :: SealedUnitypeExpression name val -> UnitypeExpression name val val
unSealedUnitypeExpression (MkSealedUnitypeExpression expr) = expr

abstractSealedUnitypeExpression ::
       Eq name => ((val -> val) -> val) -> name -> SealedUnitypeExpression name val -> SealedUnitypeExpression name val
abstractSealedUnitypeExpression tofunc name (MkSealedUnitypeExpression expr) =
    MkSealedUnitypeExpression $ fmap tofunc $ abstractUniNamedExpression name expr

varSealedUnitypeExpression :: name -> SealedUnitypeExpression name val
varSealedUnitypeExpression name = MkSealedUnitypeExpression $ varExpression $ MkNamedWitness name Refl

evalSealedUnitypeExpression :: (MonadFail m, Show name) => SealedUnitypeExpression name val -> m val
evalSealedUnitypeExpression expr = do
    MkAny Refl a <- evalSealedExpression expr
    return a

letSealedUnitypeExpression ::
       Eq name
    => name
    -> SealedUnitypeExpression name val
    -> SealedUnitypeExpression name val
    -> SealedUnitypeExpression name val
letSealedUnitypeExpression name (MkSealedUnitypeExpression val) (MkSealedUnitypeExpression body) =
    MkSealedUnitypeExpression $ letExpression (uniNamedMatch name) val body

type UnitypeBindings name val = Bindings name ((:~:) val) ((:~:) val)

uncheckedBindingsLetExpression ::
       forall name val. Eq name
    => UnitypeBindings name val
    -> SealedUnitypeExpression name val
    -> SealedUnitypeExpression name val
uncheckedBindingsLetExpression (MkBindings bb) (MkSealedUnitypeExpression body) = let
    appCons vva vv = vva (head vv) (tail vv)
    abstractList :: [name] -> UnitypeExpression name val t -> UnitypeExpression name val ([val] -> t)
    abstractList [] expr = fmap (\a _ -> a) expr
    abstractList (n:nn) expr = fmap appCons $ abstractUniNamedExpression n $ abstractList nn expr
    abstractNames :: UnitypeExpression name val t -> UnitypeExpression name val ([val] -> t)
    abstractNames = abstractList (fmap fst bb)
    exprs :: UnitypeExpression name val [val]
    exprs = fmap fix $ abstractNames $ for bb $ \(_, MkSealedUnitypeExpression b) -> b
    in MkSealedUnitypeExpression $ abstractNames body <*> exprs

bindingsLetExpression ::
       (MonadFail m, Eq name, Show name)
    => UnitypeBindings name val
    -> m (SealedUnitypeExpression name val -> SealedUnitypeExpression name val)
bindingsLetExpression bindings =
    case bindingsDuplicates bindings of
        [] -> return $ uncheckedBindingsLetExpression bindings
        l -> fail $ "duplicate bindings: " ++ intercalate ", " (fmap show l)
