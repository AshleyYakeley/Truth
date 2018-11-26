module Pinafore.Language.SimpleEntityType where

import Pinafore.Base
import Pinafore.Language.Literal
import Pinafore.Language.NamedEntity
import Pinafore.Language.Show
import Pinafore.Language.TypeContext
import Shapes
import Truth.Core

-- This doesn't include pair entity types.
data SimpleEntityType t where
    TopSimpleEntityType :: SimpleEntityType Entity
    PointSimpleEntityType :: SimpleEntityType Point
    NamedSimpleEntityType :: SymbolWitness name -> SimpleEntityType (NamedEntity name)
    LiteralSimpleEntityType :: LiteralType t -> SimpleEntityType t

instance TestEquality SimpleEntityType where
    testEquality TopSimpleEntityType TopSimpleEntityType = Just Refl
    testEquality PointSimpleEntityType PointSimpleEntityType = Just Refl
    testEquality (NamedSimpleEntityType t1) (NamedSimpleEntityType t2) = do
        Refl <- testEquality t1 t2
        return Refl
    testEquality (LiteralSimpleEntityType t1) (LiteralSimpleEntityType t2) = do
        Refl <- testEquality t1 t2
        return Refl
    testEquality _ _ = Nothing

simpleEntityToEntity :: SimpleEntityType t -> t -> Entity
simpleEntityToEntity TopSimpleEntityType = id
simpleEntityToEntity PointSimpleEntityType = pointToEntity
simpleEntityToEntity (NamedSimpleEntityType _) = namedToEntity
simpleEntityToEntity (LiteralSimpleEntityType t) =
    case literalTypeAsLiteral t of
        Dict -> pointToEntity . literalToPoint

instance TypeCheckSubtype SimpleEntityType where
    getSubtype t TopSimpleEntityType = return $ simpleEntityToEntity t
    getSubtype (LiteralSimpleEntityType t1) (LiteralSimpleEntityType t2)
        | Just conv <- isSubtype t1 t2 = return conv
    getSubtype PointSimpleEntityType PointSimpleEntityType = return id
    getSubtype PointSimpleEntityType (NamedSimpleEntityType _) = return MkNamedEntity
    getSubtype (NamedSimpleEntityType t1) (NamedSimpleEntityType t2) = getEntitySubtype t1 t2
    getSubtype t1 t2 = convertFailure (unpack $ exprShow t1) (unpack $ exprShow t2)

instance ExprShow (SimpleEntityType t) where
    exprShowPrec (LiteralSimpleEntityType t) = exprShowPrec t
    exprShowPrec PointSimpleEntityType = ("Point", 0)
    exprShowPrec TopSimpleEntityType = ("Entity", 0)
    exprShowPrec (NamedSimpleEntityType n) = (pack $ show n, 0)

simpleEntityTypeEq :: SimpleEntityType t -> Dict (Eq t)
simpleEntityTypeEq TopSimpleEntityType = Dict
simpleEntityTypeEq PointSimpleEntityType = Dict
simpleEntityTypeEq (NamedSimpleEntityType _) = Dict
simpleEntityTypeEq (LiteralSimpleEntityType lt) =
    case literalTypeAsLiteral lt of
        Dict -> Dict

simpleEntityPointAdapter :: forall t. SimpleEntityType t -> PointAdapter t
simpleEntityPointAdapter TopSimpleEntityType = bijectionPointAdapter $ MkBijection MkEntity $ \(MkEntity p) -> p
simpleEntityPointAdapter PointSimpleEntityType = bijectionPointAdapter id
simpleEntityPointAdapter (NamedSimpleEntityType _) = bijectionPointAdapter $ MkBijection MkNamedEntity unNamedEntity
simpleEntityPointAdapter (LiteralSimpleEntityType tl) =
    case literalTypeAsLiteral tl of
        Dict -> let
            pointAdapterConvert = literalToPoint
            pointAdapterGet ::
                   forall m. MonadIO m
                => Point
                -> MutableRead m PinaforePointRead
                -> m (Know t)
            pointAdapterGet p mr = do
                kl <- mr $ PinaforePointReadToLiteral p
                return $ kl >>= fromLiteral
            pointAdapterPut ::
                   forall m. MonadIO m
                => t
                -> MutableRead m PinaforePointRead
                -> m [PinaforePointEdit]
            pointAdapterPut t _mr = return [PinaforePointEditSetLiteral (literalToPoint t) (Known $ toLiteral t)]
            in MkPointAdapter {..}
