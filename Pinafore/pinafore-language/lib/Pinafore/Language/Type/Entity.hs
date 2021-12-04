{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Type.Entity
    ( entityGroundType
    , monoEntityToNegativePinaforeType
    , getMonoEntityType
    , maybeEntityConvert
    , listEntityConvert
    , pairEntityConvert
    , eitherEntityConvert
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Type.Entity.Closed
import Pinafore.Language.Type.Entity.Dynamic
import Pinafore.Language.Type.Entity.Open
import Pinafore.Language.Type.Entity.Type
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Type
import Pinafore.Language.Type.Types
import Shapes

maybeEntityAdapter :: EntityAdapter t -> EntityAdapter (Maybe t)
maybeEntityAdapter et = let
    justAnchor = codeAnchor "pinafore-base:Just"
    justAdapter = constructorEntityAdapter justAnchor $ ConsListType et NilListType
    nothingAnchor = codeAnchor "pinafore-base:Nothing"
    nothingAdapter = constructorEntityAdapter nothingAnchor NilListType
    from :: Either (a, ()) () -> Maybe a
    from (Left (a, ())) = Just a
    from (Right ()) = Nothing
    to :: Maybe a -> Either (a, ()) ()
    to (Just a) = Left (a, ())
    to Nothing = Right ()
    in isoMap from to $ justAdapter <+++> nothingAdapter

maybeEntityConvert :: Maybe Entity -> Entity
maybeEntityConvert = entityAdapterConvert $ maybeEntityAdapter plainEntityAdapter

maybeEntityFamily :: EntityFamily
maybeEntityFamily =
    pinaforeEntityFamily maybeGroundType $ \epShowType -> let
        epCovaryMap = covarymap
        epEq :: forall (t :: Type). Arguments (MonoType EntityGroundType) Maybe t -> Dict (Eq t)
        epEq (ConsArguments t NilArguments) =
            case monoEntityTypeEq t of
                Dict -> Dict
        epAdapter :: forall t. Arguments MonoEntityType Maybe t -> EntityAdapter t
        epAdapter (ConsArguments t NilArguments) = maybeEntityAdapter $ monoEntityAdapter t
        in MkEntityProperties {..}

listEntityAdapter :: EntityAdapter t -> EntityAdapter [t]
listEntityAdapter et = let
    nilAnchor = codeAnchor "pinafore-base:Nil"
    nilAdapter = constructorEntityAdapter nilAnchor NilListType
    consAnchor = codeAnchor "pinafore-base:Cons"
    consAdapter = constructorEntityAdapter consAnchor $ ConsListType et $ ConsListType listAdapter NilListType
    listAdapter = isoMap from to $ nilAdapter <+++> consAdapter
    from :: Either () (a, ([a], ())) -> [a]
    from (Left ()) = []
    from (Right (a, (aa, ()))) = a : aa
    to :: [a] -> Either () (a, ([a], ()))
    to [] = Left ()
    to (a:aa) = Right (a, (aa, ()))
    in listAdapter

listEntityConvert :: [Entity] -> Entity
listEntityConvert = entityAdapterConvert $ listEntityAdapter plainEntityAdapter

listEntityFamily :: EntityFamily
listEntityFamily =
    pinaforeEntityFamily listGroundType $ \epShowType -> let
        epCovaryMap = covarymap
        epEq :: forall (t :: Type). Arguments (MonoType EntityGroundType) [] t -> Dict (Eq t)
        epEq (ConsArguments t NilArguments) =
            case monoEntityTypeEq t of
                Dict -> Dict
        epAdapter :: forall t. Arguments MonoEntityType [] t -> EntityAdapter t
        epAdapter (ConsArguments t NilArguments) = listEntityAdapter $ monoEntityAdapter t
        in MkEntityProperties {..}

pairEntityAdapter :: EntityAdapter ta -> EntityAdapter tb -> EntityAdapter (ta, tb)
pairEntityAdapter eta etb = let
    pairAnchor = codeAnchor "pinafore-base:Pair"
    pairAdapter = constructorEntityAdapter pairAnchor $ ConsListType eta $ ConsListType etb NilListType
    from :: (a, (b, ())) -> (a, b)
    from (a, (b, ())) = (a, b)
    to :: (a, b) -> (a, (b, ()))
    to (a, b) = (a, (b, ()))
    in isoMap from to pairAdapter

pairEntityConvert :: (Entity, Entity) -> Entity
pairEntityConvert = entityAdapterConvert $ pairEntityAdapter plainEntityAdapter plainEntityAdapter

pairEntityFamily :: EntityFamily
pairEntityFamily =
    pinaforeEntityFamily pairGroundType $ \epShowType -> let
        epCovaryMap = covarymap
        epEq :: forall (t :: Type). Arguments (MonoType EntityGroundType) (,) t -> Dict (Eq t)
        epEq (ConsArguments ta (ConsArguments tb NilArguments)) =
            case (monoEntityTypeEq ta, monoEntityTypeEq tb) of
                (Dict, Dict) -> Dict
        epAdapter :: forall t. Arguments MonoEntityType (,) t -> EntityAdapter t
        epAdapter (ConsArguments ta (ConsArguments tb NilArguments)) =
            pairEntityAdapter (monoEntityAdapter ta) (monoEntityAdapter tb)
        in MkEntityProperties {..}

eitherEntityAdapter :: EntityAdapter ta -> EntityAdapter tb -> EntityAdapter (Either ta tb)
eitherEntityAdapter eta etb = let
    from :: (a, ()) -> a
    from (a, ()) = a
    to :: a -> (a, ())
    to a = (a, ())
    leftAnchor = codeAnchor "pinafore-base:Left"
    leftAdapter = isoMap from to $ constructorEntityAdapter leftAnchor $ ConsListType eta NilListType
    rightAnchor = codeAnchor "pinafore-base:Right"
    rightAdapter = isoMap from to $ constructorEntityAdapter rightAnchor $ ConsListType etb NilListType
    in leftAdapter <+++> rightAdapter

eitherEntityConvert :: Either Entity Entity -> Entity
eitherEntityConvert = entityAdapterConvert $ eitherEntityAdapter plainEntityAdapter plainEntityAdapter

eitherEntityFamily :: EntityFamily
eitherEntityFamily =
    pinaforeEntityFamily eitherGroundType $ \epShowType -> let
        epCovaryMap = covarymap
        epEq :: forall (t :: Type). Arguments (MonoType EntityGroundType) Either t -> Dict (Eq t)
        epEq (ConsArguments ta (ConsArguments tb NilArguments)) =
            case (monoEntityTypeEq ta, monoEntityTypeEq tb) of
                (Dict, Dict) -> Dict
        epAdapter :: forall t. Arguments MonoEntityType Either t -> EntityAdapter t
        epAdapter (ConsArguments ta (ConsArguments tb NilArguments)) =
            eitherEntityAdapter (monoEntityAdapter ta) (monoEntityAdapter tb)
        in MkEntityProperties {..}

entityGroundType :: PinaforeGroundType '[] Entity
entityGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Entity)|]) "Entity"

entityEntityFamily :: EntityFamily
entityEntityFamily = simplePinaforeEntityFamily entityGroundType plainEntityAdapter

literalEntityFamily ::
       forall (t :: Type). AsLiteral t
    => PinaforeGroundType '[] t
    -> EntityFamily
literalEntityFamily t =
    pinaforeEntityFamily t $ \epShowType -> let
        epCovaryMap = covarymap
        epEq :: forall (ta :: Type). Arguments (MonoType EntityGroundType) t ta -> Dict (Eq ta)
        epEq NilArguments = Dict
        epAdapter :: forall ta. Arguments MonoEntityType t ta -> EntityAdapter ta
        epAdapter NilArguments = literalEntityAdapter
        in MkEntityProperties {..}

findmap :: [a] -> (a -> Maybe b) -> Maybe b
findmap [] _ = Nothing
findmap (x:xs) f =
    case f x of
        Nothing -> findmap xs f
        Just y -> Just y

allEntityFamilies :: [EntityFamily]
allEntityFamilies =
    [ maybeEntityFamily
    , listEntityFamily
    , pairEntityFamily
    , eitherEntityFamily
    , entityEntityFamily
    , dynamicEntityFamily
    , aDynamicEntityEntityFamily
    , closedEntityFamily
    , openEntityFamily
    , literalEntityFamily literalGroundType
    , literalEntityFamily unitGroundType
    , literalEntityFamily textGroundType
    , literalEntityFamily numberGroundType
    , literalEntityFamily rationalGroundType
    , literalEntityFamily integerGroundType
    , literalEntityFamily booleanGroundType
    , literalEntityFamily orderingGroundType
    , literalEntityFamily timeGroundType
    , literalEntityFamily durationGroundType
    , literalEntityFamily dateGroundType
    , literalEntityFamily timeOfDayGroundType
    , literalEntityFamily localTimeGroundType
    ]

instance CovarySubtype PinaforeGroundType EntityGroundType where
    dolanToMonoGroundType ::
           forall (dv :: DolanVariance) (t :: DolanVarianceKind dv).
           PinaforeGroundType dv t
        -> Maybe (CovaryType dv, EntityGroundType t)
    dolanToMonoGroundType MkPinaforeGroundType {..} =
        findmap allEntityFamilies $ \(MkEntityFamily wit ff) -> do
            tt <- matchFamilyType wit pgtFamilyType
            eprops <- ff pgtVarianceType tt
            covaryType <- dolanVarianceToCovaryType pgtVarianceType
            return (covaryType, MkEntityGroundType pgtFamilyType covaryType eprops)
    monoToDolanGroundType ::
           forall (dv :: DolanVariance) (t :: DolanVarianceKind dv).
           CovaryType dv
        -> EntityGroundType t
        -> PinaforeGroundType dv t
    monoToDolanGroundType = entityToPinaforeGroundType

monoEntityToNegativePinaforeType ::
       forall m t. MonadThrow ErrorType m
    => MonoEntityType t
    -> m (PinaforeShimWit 'Negative t)
monoEntityToNegativePinaforeType et =
    case monoToMaybeNegativeDolanType et of
        Just wit -> return wit
        Nothing -> throw InterpretTypeNoneNotNegativeEntityError

getMonoEntityType :: MonadThrow ErrorType m => AnyW (PinaforeType 'Positive) -> m (AnyW MonoEntityType)
getMonoEntityType (MkAnyW tm) =
    case dolanToMonoType tm of
        Just (MkShimWit t _) -> return $ MkAnyW t
        Nothing -> throw $ InterpretTypeNotEntityError $ exprShow tm
