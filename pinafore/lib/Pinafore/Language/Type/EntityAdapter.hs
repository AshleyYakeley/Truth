module Pinafore.Language.Type.EntityAdapter
    ( concreteEntityAdapter
    , concreteToEntityShim
    ) where

import Data.Shim
import Language.Expression.Common
import Pinafore.Base
import Pinafore.Language.Shim
import Pinafore.Language.Type.Entity
import Pinafore.Language.Type.Literal
import Pinafore.Language.Value
import Shapes
import Truth.Core

entityGroundTypeAdapter :: forall f t. EntityGroundType f -> Arguments ConcreteEntityType f t -> EntityAdapter t
entityGroundTypeAdapter TopEntityGroundType NilArguments = entityEntityAdapter
entityGroundTypeAdapter NewEntityGroundType NilArguments = isoMap MkNewEntity unNewEntity entityEntityAdapter
entityGroundTypeAdapter (OpenEntityGroundType _ _) NilArguments = isoMap MkOpenEntity unNamedEntity entityEntityAdapter
entityGroundTypeAdapter (LiteralEntityGroundType tl) NilArguments =
    case literalTypeAsLiteral tl of
        Dict -> let
            entityAdapterConvert = literalToEntity
            entityAdapterGet :: Entity -> ReadM PinaforeEntityRead (Know t)
            entityAdapterGet p = do
                kl <- readM $ PinaforeEntityReadToLiteral p
                return $ kl >>= fromLiteral
            entityAdapterPut :: t -> ReadM PinaforeEntityRead [PinaforeEntityEdit]
            entityAdapterPut t = return [PinaforeEntityEditSetLiteral (literalToEntity t) (Known $ toLiteral t)]
            in MkEntityAdapter {..}
entityGroundTypeAdapter MaybeEntityGroundType (ConsArguments t NilArguments) = let
    justAnchor = codeAnchor "pinafore-base:Just"
    justAdapter = constructorEntityAdapter justAnchor $ ConsListType (concreteEntityAdapter t) NilListType
    nothingAnchor = codeAnchor "pinafore-base:Nothing"
    nothingAdapter = constructorEntityAdapter nothingAnchor NilListType
    from :: Either (a, ()) () -> Maybe a
    from (Left (a, ())) = Just a
    from (Right ()) = Nothing
    to :: Maybe a -> Either (a, ()) ()
    to (Just a) = Left (a, ())
    to Nothing = Right ()
    in isoMap from to $ justAdapter <+++> nothingAdapter
entityGroundTypeAdapter PairEntityGroundType (ConsArguments ta (ConsArguments tb NilArguments)) = let
    pairAnchor = codeAnchor "pinafore-base:Pair"
    pairAdapter =
        constructorEntityAdapter pairAnchor $
        ConsListType (concreteEntityAdapter ta) $ ConsListType (concreteEntityAdapter tb) NilListType
    from :: (a, (b, ())) -> (a, b)
    from (a, (b, ())) = (a, b)
    to :: (a, b) -> (a, (b, ()))
    to (a, b) = (a, (b, ()))
    in isoMap from to pairAdapter
entityGroundTypeAdapter EitherEntityGroundType (ConsArguments ta (ConsArguments tb NilArguments)) = let
    from :: (a, ()) -> a
    from (a, ()) = a
    to :: a -> (a, ())
    to a = (a, ())
    leftAnchor = codeAnchor "pinafore-base:Left"
    leftAdapter =
        isoMap from to $ constructorEntityAdapter leftAnchor $ ConsListType (concreteEntityAdapter ta) NilListType
    rightAnchor = codeAnchor "pinafore-base:Right"
    rightAdapter =
        isoMap from to $ constructorEntityAdapter rightAnchor $ ConsListType (concreteEntityAdapter tb) NilListType
    in leftAdapter <+++> rightAdapter
entityGroundTypeAdapter ListEntityGroundType (ConsArguments t NilArguments) = let
    nilAnchor = codeAnchor "pinafore-base:Nil"
    nilAdapter = constructorEntityAdapter nilAnchor NilListType
    consAnchor = codeAnchor "pinafore-base:Cons"
    consAdapter =
        constructorEntityAdapter consAnchor $
        ConsListType (concreteEntityAdapter t) $ ConsListType listAdapter NilListType
    listAdapter = isoMap from to $ nilAdapter <+++> consAdapter
    from :: Either () (a, ([a], ())) -> [a]
    from (Left ()) = []
    from (Right (a, (aa, ()))) = a : aa
    to :: [a] -> Either () (a, ([a], ()))
    to [] = Left ()
    to (a:aa) = Right (a, (aa, ()))
    in listAdapter
entityGroundTypeAdapter (ClosedEntityGroundType _ _ ct) NilArguments = closedEntityTypeAdapter ct

closedEntityTypeAdapter :: ClosedEntityType t -> EntityAdapter t
closedEntityTypeAdapter NilClosedEntityType = pNone
closedEntityTypeAdapter (ConsClosedEntityType a cc rest) =
    constructorEntityAdapter a (mapListType concreteEntityAdapter cc) <+++> closedEntityTypeAdapter rest

concreteEntityAdapter :: forall t. ConcreteEntityType t -> EntityAdapter t
concreteEntityAdapter (MkConcreteType gt args) = entityGroundTypeAdapter gt args

concreteToEntityShim :: ConcreteEntityType a -> PinaforePolyShim Type a Entity
concreteToEntityShim (MkConcreteType TopEntityGroundType NilArguments) = id
concreteToEntityShim (MkConcreteType NewEntityGroundType NilArguments) = coerceEnhanced "subtype"
concreteToEntityShim (MkConcreteType (OpenEntityGroundType _ _) NilArguments) = coerceEnhanced "subtype"
concreteToEntityShim t = functionToShim "subtype" $ entityAdapterConvert $ concreteEntityAdapter t
