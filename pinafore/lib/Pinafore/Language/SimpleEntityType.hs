module Pinafore.Language.SimpleEntityType where

import Pinafore.Base
import Pinafore.Language.Literal
import Pinafore.Language.NamedEntity
import Pinafore.Language.Show
import Pinafore.Language.TypeContext
import Shapes
import Truth.Core

-- This doesn't include pair or Either entity types.
data SimpleEntityType t where
    TopSimpleEntityType :: SimpleEntityType Entity
    NewSimpleEntityType :: SimpleEntityType NewEntity
    NamedSimpleEntityType :: SymbolWitness name -> SimpleEntityType (NamedEntity name)
    LiteralSimpleEntityType :: LiteralType t -> SimpleEntityType t

instance TestEquality SimpleEntityType where
    testEquality TopSimpleEntityType TopSimpleEntityType = Just Refl
    testEquality NewSimpleEntityType NewSimpleEntityType = Just Refl
    testEquality (NamedSimpleEntityType t1) (NamedSimpleEntityType t2) = do
        Refl <- testEquality t1 t2
        return Refl
    testEquality (LiteralSimpleEntityType t1) (LiteralSimpleEntityType t2) = do
        Refl <- testEquality t1 t2
        return Refl
    testEquality _ _ = Nothing

simpleEntityToEntity :: SimpleEntityType t -> t -> Entity
simpleEntityToEntity et = entityAdapterConvert $ simpleEntityAdapter et

instance TypeCheckSubtype SimpleEntityType where
    getSubtype t TopSimpleEntityType = return $ simpleEntityToEntity t
    getSubtype (LiteralSimpleEntityType t1) (LiteralSimpleEntityType t2)
        | Just conv <- isSubtype t1 t2 = return conv
    getSubtype NewSimpleEntityType NewSimpleEntityType = return id
    getSubtype NewSimpleEntityType (NamedSimpleEntityType _) = return $ MkNamedEntity . unNewEntity
    getSubtype (NamedSimpleEntityType t1) (NamedSimpleEntityType t2) = getEntitySubtype t1 t2
    getSubtype t1 t2 = convertFailure (unpack $ exprShow t1) (unpack $ exprShow t2)

instance ExprShow (SimpleEntityType t) where
    exprShowPrec (LiteralSimpleEntityType t) = exprShowPrec t
    exprShowPrec NewSimpleEntityType = ("NewEntity", 0)
    exprShowPrec TopSimpleEntityType = ("Entity", 0)
    exprShowPrec (NamedSimpleEntityType n) = (pack $ show n, 0)

simpleEntityTypeEq :: SimpleEntityType t -> Dict (Eq t)
simpleEntityTypeEq TopSimpleEntityType = Dict
simpleEntityTypeEq NewSimpleEntityType = Dict
simpleEntityTypeEq (NamedSimpleEntityType _) = Dict
simpleEntityTypeEq (LiteralSimpleEntityType lt) =
    case literalTypeAsLiteral lt of
        Dict -> Dict

simpleEntityAdapter :: forall t. SimpleEntityType t -> EntityAdapter t
simpleEntityAdapter TopSimpleEntityType = bijectionEntityAdapter id id
simpleEntityAdapter NewSimpleEntityType = bijectionEntityAdapter MkNewEntity unNewEntity
simpleEntityAdapter (NamedSimpleEntityType _) = bijectionEntityAdapter MkNamedEntity unNamedEntity
simpleEntityAdapter (LiteralSimpleEntityType tl) =
    case literalTypeAsLiteral tl of
        Dict -> let
            entityAdapterConvert = literalToEntity
            entityAdapterGet ::
                   forall m. MonadIO m
                => Entity
                -> MutableRead m PinaforeEntityRead
                -> m (Know t)
            entityAdapterGet p mr = do
                kl <- mr $ PinaforeEntityReadToLiteral p
                return $ kl >>= fromLiteral
            entityAdapterPut ::
                   forall m. MonadIO m
                => t
                -> MutableRead m PinaforeEntityRead
                -> m [PinaforeEntityEdit]
            entityAdapterPut t _mr = return [PinaforeEntityEditSetLiteral (literalToEntity t) (Known $ toLiteral t)]
            in MkEntityAdapter {..}
