module Pinafore.Language.EntityType where

import Pinafore.Base
import Pinafore.Language.Type
import Shapes
import Text.Read
import Truth.Core

data EntityType t where
    SimpleEntityType :: SimpleEntityType t -> EntityType t
    PairEntityType :: EntityType ta -> EntityType tb -> EntityType (ta, tb)
    EitherEntityType :: EntityType ta -> EntityType tb -> EntityType (Either ta tb)

instance TestEquality EntityType where
    testEquality (SimpleEntityType t1) (SimpleEntityType t2) = do
        Refl <- testEquality t1 t2
        return Refl
    testEquality (PairEntityType t1a t1b) (PairEntityType t2a t2b) = do
        Refl <- testEquality t1a t2a
        Refl <- testEquality t1b t2b
        return Refl
    testEquality (EitherEntityType t1a t1b) (EitherEntityType t2a t2b) = do
        Refl <- testEquality t1a t2a
        Refl <- testEquality t1b t2b
        return Refl
    testEquality _ _ = Nothing

instance Show (EntityType t) where
    show et =
        case entityTypeToType @_ @'PositivePolarity et of
            MkTypeF t _ -> show t

entityTypeEq :: EntityType t -> Dict (Eq t)
entityTypeEq (SimpleEntityType st) = simpleEntityTypeEq st
entityTypeEq (PairEntityType ta tb) =
    case (entityTypeEq ta, entityTypeEq tb) of
        (Dict, Dict) -> Dict
entityTypeEq (EitherEntityType ta tb) =
    case (entityTypeEq ta, entityTypeEq tb) of
        (Dict, Dict) -> Dict

entityTypeToType ::
       forall baseedit polarity t. IsTypePolarity polarity
    => EntityType t
    -> PinaforeTypeF baseedit polarity t
entityTypeToType (SimpleEntityType t) =
    singlePinaforeTypeF $ mkTypeF $ GroundPinaforeSingularType (SimpleEntityPinaforeGroundType t) NilDolanArguments
entityTypeToType (PairEntityType eta etb) = let
    taf = entityTypeToType @baseedit @polarity eta
    tbf = entityTypeToType @baseedit @polarity etb
    in case whichTypePolarity @polarity of
           Left Refl ->
               unTypeF taf $ \ta conva ->
                   unTypeF tbf $ \tb convb ->
                       singlePinaforeTypeF $
                       contramap (\(a, b) -> (conva a, convb b)) $
                       mkTypeF $
                       GroundPinaforeSingularType PairPinaforeGroundType $
                       ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
           Right Refl ->
               unTypeF taf $ \ta conva ->
                   unTypeF tbf $ \tb convb ->
                       singlePinaforeTypeF $
                       fmap (\(a, b) -> (conva a, convb b)) $
                       mkTypeF $
                       GroundPinaforeSingularType PairPinaforeGroundType $
                       ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
entityTypeToType (EitherEntityType eta etb) = let
    taf = entityTypeToType @baseedit @polarity eta
    tbf = entityTypeToType @baseedit @polarity etb
    in case whichTypePolarity @polarity of
           Left Refl ->
               unTypeF taf $ \ta conva ->
                   unTypeF tbf $ \tb convb ->
                       singlePinaforeTypeF $
                       contramap (either (Left . conva) (Right . convb)) $
                       mkTypeF $
                       GroundPinaforeSingularType EitherPinaforeGroundType $
                       ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
           Right Refl ->
               unTypeF taf $ \ta conva ->
                   unTypeF tbf $ \tb convb ->
                       singlePinaforeTypeF $
                       fmap (either (Left . conva) (Right . convb)) $
                       mkTypeF $
                       GroundPinaforeSingularType EitherPinaforeGroundType $
                       ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

singularTypeToEntityType :: PinaforeSingularType baseedit polarity t -> Maybe (AnyW EntityType)
singularTypeToEntityType (GroundPinaforeSingularType (SimpleEntityPinaforeGroundType st) NilDolanArguments) =
    Just $ MkAnyW $ SimpleEntityType st
singularTypeToEntityType (GroundPinaforeSingularType PairPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments))) = do
    MkAnyW eta <- typeToEntityType ta
    MkAnyW etb <- typeToEntityType tb
    return $ MkAnyW $ PairEntityType eta etb
singularTypeToEntityType (GroundPinaforeSingularType EitherPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments))) = do
    MkAnyW eta <- typeToEntityType ta
    MkAnyW etb <- typeToEntityType tb
    return $ MkAnyW $ EitherEntityType eta etb
singularTypeToEntityType _ = Nothing

typeToEntityType :: forall baseedit polarity t. PinaforeType baseedit polarity t -> Maybe (AnyW EntityType)
typeToEntityType (ConsPinaforeType st NilPinaforeType) = singularTypeToEntityType st
typeToEntityType _ = Nothing

predfst :: Predicate
predfst = MkPredicate $ MkAnchor $ read "9fa17b88-89f3-4baf-b4b3-fdb7280a0020"

predsnd :: Predicate
predsnd = MkPredicate $ MkAnchor $ read "3c667db3-c3a5-4ef9-9b15-6cad178c50c7"

predleft :: Predicate
predleft = MkPredicate $ MkAnchor $ read "ffb8fee1-6971-46c0-9954-62c2ec53e98a"

predright :: Predicate
predright = MkPredicate $ MkAnchor $ read "bbc7a8ca-17e1-4d42-9230-e6b889dea2e5"

--unitPoint :: Entity
--unitPoint = MkEntity $ MkAnchor $ read "644eaa9b-0c57-4c5c-9606-e5303fda86f9"
entityAdapter :: EntityType t -> EntityAdapter t
entityAdapter (SimpleEntityType t) = simpleEntityAdapter t
entityAdapter (PairEntityType ta tb) = let
    MkEntityAdapter ae aget aput = entityAdapter ta
    MkEntityAdapter be bget bput = entityAdapter tb
    entityAdapterConvert (a, b) = pairToEntity (ae a, be b)
    entityAdapterGet ::
           forall m. MonadIO m
        => Entity
        -> MutableRead m PinaforeEntityRead
        -> m (Know _)
    entityAdapterGet p mr =
        getComposeM $ do
            pa <- MkComposeM $ mr $ PinaforeEntityReadGetPredicate predfst p
            a <- MkComposeM $ aget pa mr
            pb <- MkComposeM $ mr $ PinaforeEntityReadGetPredicate predsnd p
            b <- MkComposeM $ bget pb mr
            return (a, b)
    entityAdapterPut ::
           forall m. MonadIO m
        => _
        -> MutableRead m PinaforeEntityRead
        -> m [PinaforeEntityEdit]
    entityAdapterPut ab mr = let
        pab = entityAdapterConvert ab
        in case ab of
               (a, b) -> do
                   aedits <- aput a mr
                   bedits <- bput b mr
                   return $
                       aedits <>
                       bedits <>
                       [ PinaforeEntityEditSetPredicate predfst pab $ Known $ ae a
                       , PinaforeEntityEditSetPredicate predsnd pab $ Known $ be b
                       ]
    in MkEntityAdapter {..}
entityAdapter (EitherEntityType ta tb) = let
    MkEntityAdapter ae aget aput = entityAdapter ta
    MkEntityAdapter be bget bput = entityAdapter tb
    entityAdapterConvert eab = eitherToEntity $ either (Left . ae) (Right . be) eab
    entityAdapterGet ::
           forall m. MonadIO m
        => Entity
        -> MutableRead m PinaforeEntityRead
        -> m (Know _)
    entityAdapterGet p mr =
        getComposeM $
        (do
             pa <- MkComposeM $ mr $ PinaforeEntityReadGetPredicate predleft p
             a <- MkComposeM $ aget pa mr
             return $ Left a) <|>
        (do
             pb <- MkComposeM $ mr $ PinaforeEntityReadGetPredicate predright p
             b <- MkComposeM $ bget pb mr
             return $ Right b)
    entityAdapterPut ::
           forall m. MonadIO m
        => _
        -> MutableRead m PinaforeEntityRead
        -> m [PinaforeEntityEdit]
    entityAdapterPut ab mr = let
        pab = entityAdapterConvert ab
        in case ab of
               Left a -> do
                   aedits <- aput a mr
                   return $ aedits <> [PinaforeEntityEditSetPredicate predleft pab $ Known $ ae a]
               Right b -> do
                   bedits <- bput b mr
                   return $ bedits <> [PinaforeEntityEditSetPredicate predright pab $ Known $ be b]
    in MkEntityAdapter {..}
{-
data EntityAdapter t = MkEntityAdapter
    { entityAdapterConvert :: t -> Entity
    , entityAdapterGet :: forall m. MonadIO m => Entity -> MutableRead m PinaforeEntityRead -> m (Know t)
    , entityAdapterPut :: forall m. MonadIO m => t -> MutableRead m PinaforeEntityRead -> m [PinaforeEntityEdit]
    }
-}
