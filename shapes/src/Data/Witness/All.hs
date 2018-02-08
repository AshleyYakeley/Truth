module Data.Witness.All where

import Shapes.Import

class AllWitnessConstraint (c :: kw -> Constraint) (w :: kt -> kw) where
    allWitnessConstraint :: forall (t :: kt). Dict (c (w t))

showAllWitness ::
       forall w t. AllWitnessConstraint Show w
    => w t
    -> String
showAllWitness wt =
    case allWitnessConstraint @_ @_ @Show @w @t of
        Dict -> show wt

newtype All (w :: * -> *) = MkAll
    { getAll :: forall t. w t -> t
    }

newtype AllF (w :: k -> *) (f :: k -> *) = MkAllF
    { getAllF :: forall (t :: k). w t -> f t
    }

allFToAll :: AllF w Identity -> All w
allFToAll (MkAllF wtit) = MkAll $ \wt -> runIdentity $ wtit wt

allToAllF :: All w -> AllF w Identity
allToAllF (MkAll wtt) = MkAllF $ \wt -> Identity $ wtt wt

class WitnessConstraint (c :: k -> Constraint) (w :: k -> *) where
    witnessConstraint :: forall (t :: k). w t -> Dict (c t)

class FiniteWitness (w :: k -> *) where
    assembleWitnessF :: Applicative m => (forall t. w t -> m (f t)) -> m (AllF w f)

allWitnesses :: FiniteWitness w => [AnyWitness w]
allWitnesses = getConst $ assembleWitnessF $ \wt -> Const [MkAnyWitness wt]

assembleWitness :: (FiniteWitness w, Applicative m) => (forall t. w t -> m t) -> m (All w)
assembleWitness wtmt = fmap allFToAll $ assembleWitnessF $ \wt -> fmap Identity $ wtmt wt

mapAnyWitness :: (forall t. w1 t -> w2 t) -> AnyWitness w1 -> AnyWitness w2
mapAnyWitness f (MkAnyWitness wt) = MkAnyWitness $ f wt

newtype EmptyWitness t =
    MkEmptyWitness None
    deriving (Eq, Countable, Searchable, Empty)

instance Finite (EmptyWitness t) where
    allValues = []
    assemble _ = pure never

instance TestEquality EmptyWitness where
    testEquality = never

instance FiniteWitness EmptyWitness where
    assembleWitnessF _ = pure emptyAllF

instance WitnessConstraint c EmptyWitness where
    witnessConstraint = never

emptyAll :: All EmptyWitness
emptyAll = MkAll never

emptyAllF :: AllF EmptyWitness f
emptyAllF = MkAllF never

data ConsWitness a r t where
    FirstWitness :: ConsWitness t r t
    RestWitness :: r t -> ConsWitness a r t

instance TestEquality r => TestEquality (ConsWitness a r) where
    testEquality FirstWitness FirstWitness = return Refl
    testEquality (RestWitness r1) (RestWitness r2) = do
        Refl <- testEquality r1 r2
        return Refl
    testEquality _ _ = Nothing

instance FiniteWitness r => FiniteWitness (ConsWitness a r) where
    assembleWitnessF getsel =
        (\f (MkAllF r) ->
             MkAllF $ \wt ->
                 case wt of
                     FirstWitness -> f
                     RestWitness rt -> r rt) <$>
        getsel FirstWitness <*>
        assembleWitnessF (getsel . RestWitness)

instance (c a, WitnessConstraint c r) => WitnessConstraint c (ConsWitness a r) where
    witnessConstraint FirstWitness = Dict
    witnessConstraint (RestWitness rt) =
        case witnessConstraint @_ @c rt of
            Dict -> Dict

consAll :: a -> All r -> All (ConsWitness a r)
consAll a (MkAll tup) =
    MkAll $ \esel ->
        case esel of
            FirstWitness -> a
            RestWitness sel -> tup sel

data EitherWitness (colsel1 :: k -> *) (colsel2 :: k -> *) (t :: k)
    = LeftWitness (colsel1 t)
    | RightWitness (colsel2 t)

instance (TestEquality colsel1, TestEquality colsel2) => TestEquality (EitherWitness colsel1 colsel2) where
    testEquality (LeftWitness s1) (LeftWitness s2) = do
        Refl <- testEquality s1 s2
        return Refl
    testEquality (RightWitness s1) (RightWitness s2) = do
        Refl <- testEquality s1 s2
        return Refl
    testEquality _ _ = Nothing

instance (FiniteWitness p, FiniteWitness q) => FiniteWitness (EitherWitness p q) where
    assembleWitnessF getsel =
        (\(MkAllF p) (MkAllF q) ->
             MkAllF $ \wt ->
                 case wt of
                     LeftWitness rt -> p rt
                     RightWitness rt -> q rt) <$>
        assembleWitnessF (getsel . LeftWitness) <*>
        assembleWitnessF (getsel . RightWitness)

instance (WitnessConstraint c p, WitnessConstraint c q) => WitnessConstraint c (EitherWitness p q) where
    witnessConstraint (LeftWitness rt) =
        case witnessConstraint @_ @c rt of
            Dict -> Dict
    witnessConstraint (RightWitness rt) =
        case witnessConstraint @_ @c rt of
            Dict -> Dict

eitherAll :: All sel1 -> All sel2 -> All (EitherWitness sel1 sel2)
eitherAll (MkAll tup1) (MkAll tup2) =
    MkAll $ \esel ->
        case esel of
            LeftWitness sel -> tup1 sel
            RightWitness sel -> tup2 sel

eitherAllF :: AllF sel1 f -> AllF sel2 f -> AllF (EitherWitness sel1 sel2) f
eitherAllF (MkAllF tup1) (MkAllF tup2) =
    MkAllF $ \esel ->
        case esel of
            LeftWitness sel -> tup1 sel
            RightWitness sel -> tup2 sel

data SubmapWitness (w :: k -> *) (f :: k -> *) = MkSubmapWitness
    { subWitnessDomain :: [AnyWitness w]
    , subWitnessMap :: forall (t :: k). w t -> f t
    }

subWitnessCodomain :: SubmapWitness w f -> [AnyWitness f]
subWitnessCodomain schema =
    fmap (\(MkAnyWitness st) -> MkAnyWitness $ subWitnessMap schema st) $ subWitnessDomain schema

mapSubmapWitness :: (forall t. f1 t -> f2 t) -> SubmapWitness w f1 -> SubmapWitness w f2
mapSubmapWitness ff (MkSubmapWitness ai i) = MkSubmapWitness ai $ \s -> ff $ i s

eitherSubmapWitness ::
       SubmapWitness sel1 itemSchema
    -> SubmapWitness sel2 itemSchema
    -> SubmapWitness (EitherWitness sel1 sel2) itemSchema
eitherSubmapWitness (MkSubmapWitness a1 i1) (MkSubmapWitness a2 i2) =
    MkSubmapWitness
        ((fmap (mapAnyWitness LeftWitness) a1) ++ (fmap (mapAnyWitness RightWitness) a2))
        (getAllF $ eitherAllF (MkAllF i1) (MkAllF i2))

finiteSubmapWitness :: FiniteWitness w => (forall t. w t -> f t) -> SubmapWitness w f
finiteSubmapWitness wf = MkSubmapWitness allWitnesses wf

type SingleWitness = (:~:)

instance FiniteWitness (SingleWitness t) where
    assembleWitnessF getsel = fmap (\ft -> MkAllF $ \Refl -> ft) $ getsel Refl

instance c t => WitnessConstraint c (SingleWitness t) where
    witnessConstraint Refl = Dict

singleAll :: t -> All (SingleWitness t)
singleAll t = MkAll $ \Refl -> t

getSingleAll :: All (SingleWitness t) -> t
getSingleAll (MkAll f) = f Refl

splitWitnessList :: TestEquality w => [Any w] -> AllF w []
splitWitnessList [] = MkAllF $ \_ -> []
splitWitnessList ((MkAny wt t):rr) =
    MkAllF $ \wt' ->
        case testEquality wt wt' of
            Just Refl -> t : (getAllF (splitWitnessList rr) wt')
            Nothing -> getAllF (splitWitnessList rr) wt'
