module Pinafore.Language.Type.EntityAdapter
    ( monoEntityAdapter
    , monoToEntityShim
    , closedEntityShim
    , entitySubtypeShim
    ) where

import Data.Shim
import Language.Expression.Common
import Pinafore.Base
import Pinafore.Language.Shim
import Pinafore.Language.Type.DynamicEntity
import Pinafore.Language.Type.Entity
import Pinafore.Language.Type.Literal
import Pinafore.Language.Type.OpenEntity
import Shapes

entityGroundTypeAdapter :: forall f t. EntityGroundType f -> Arguments MonoEntityType f t -> EntityAdapter t
entityGroundTypeAdapter TopEntityGroundType NilArguments = plainEntityAdapter
entityGroundTypeAdapter (OpenEntityGroundType _) NilArguments = isoMap MkOpenEntity unOpenEntity plainEntityAdapter
entityGroundTypeAdapter (LiteralEntityGroundType tl) NilArguments =
    case literalTypeAsLiteral tl of
        Dict -> literalEntityAdapter
entityGroundTypeAdapter MaybeEntityGroundType (ConsArguments t NilArguments) = let
    justAnchor = codeAnchor "pinafore-base:Just"
    justAdapter = constructorEntityAdapter justAnchor $ ConsListType (monoEntityAdapter t) NilListType
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
        ConsListType (monoEntityAdapter ta) $ ConsListType (monoEntityAdapter tb) NilListType
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
    leftAdapter = isoMap from to $ constructorEntityAdapter leftAnchor $ ConsListType (monoEntityAdapter ta) NilListType
    rightAnchor = codeAnchor "pinafore-base:Right"
    rightAdapter =
        isoMap from to $ constructorEntityAdapter rightAnchor $ ConsListType (monoEntityAdapter tb) NilListType
    in leftAdapter <+++> rightAdapter
entityGroundTypeAdapter ListEntityGroundType (ConsArguments t NilArguments) = let
    nilAnchor = codeAnchor "pinafore-base:Nil"
    nilAdapter = constructorEntityAdapter nilAnchor NilListType
    consAnchor = codeAnchor "pinafore-base:Cons"
    consAdapter =
        constructorEntityAdapter consAnchor $ ConsListType (monoEntityAdapter t) $ ConsListType listAdapter NilListType
    listAdapter = isoMap from to $ nilAdapter <+++> consAdapter
    from :: Either () (a, ([a], ())) -> [a]
    from (Left ()) = []
    from (Right (a, (aa, ()))) = a : aa
    to :: [a] -> Either () (a, ([a], ()))
    to [] = Left ()
    to (a:aa) = Right (a, (aa, ()))
    in listAdapter
entityGroundTypeAdapter (ClosedEntityGroundType _ _ ct) NilArguments = closedEntityTypeAdapter ct
entityGroundTypeAdapter TopDynamicEntityGroundType NilArguments = dynamicEntityAdapter Nothing
entityGroundTypeAdapter (ADynamicEntityGroundType _ dt) NilArguments = dynamicEntityAdapter $ Just dt

closedEntityTypeAdapter :: ClosedEntityType t -> EntityAdapter t
closedEntityTypeAdapter NilClosedEntityType = pNone
closedEntityTypeAdapter (ConsClosedEntityType a cc rest) =
    constructorEntityAdapter a (mapListType monoEntityAdapter cc) <+++> closedEntityTypeAdapter rest

closedEntityShim :: ClosedEntityType a -> PinaforePolyShim Type a Entity
closedEntityShim t = functionToShim "ClosedEntity to Entity" $ entityAdapterConvert $ closedEntityTypeAdapter t

monoEntityAdapter :: forall t. MonoEntityType t -> EntityAdapter t
monoEntityAdapter (MkMonoType gt args) = entityGroundTypeAdapter gt args

monoToEntityShim :: MonoEntityType a -> PinaforePolyShim Type a Entity
monoToEntityShim (MkMonoType TopEntityGroundType NilArguments) = id
monoToEntityShim (MkMonoType (OpenEntityGroundType _) NilArguments) = coerceEnhanced "subtype"
monoToEntityShim t = functionToShim "subtype" $ entityAdapterConvert $ monoEntityAdapter t

entitySubtypeShim :: forall (t :: Type). EntityGroundType t -> PinaforePolyShim Type t Entity
entitySubtypeShim TopEntityGroundType = id
entitySubtypeShim (OpenEntityGroundType _) = coerceEnhanced "open entity"
entitySubtypeShim (LiteralEntityGroundType t) =
    case literalTypeAsLiteral t of
        Dict -> functionToShim "literal to Entity" literalToEntity
entitySubtypeShim (ClosedEntityGroundType _ _ t) = closedEntityShim t
entitySubtypeShim t = functionToShim "to Entity" $ entityAdapterConvert $ entityGroundTypeAdapter t NilArguments
