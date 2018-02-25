{-# OPTIONS -fno-warn-orphans #-}

module Data.Witness.All where

import Shapes.Import

class AllWitnessConstraint (c :: kw -> Constraint) (w :: kt -> kw) where
    allWitnessConstraint :: forall (t :: kt). Dict (c (w t))

instance AllWitnessConstraint Show ((:~:) t) where
    allWitnessConstraint = Dict

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

type family UnAll (aw :: *) :: * -> * where
    UnAll (All w) = w

class WitnessConstraint (c :: k -> Constraint) (w :: k -> *) where
    witnessConstraint :: forall (t :: k). w t -> Dict (c t)

class FiniteWitness (w :: k -> *) where
    assembleWitnessF :: Applicative m => (forall t. w t -> m (f t)) -> m (AllF w f)

instance (TestEquality w, FiniteWitness w) => Countable (AnyWitness w) where
    countPrevious = finiteCountPrevious
    countMaybeNext = finiteCountMaybeNext

instance (TestEquality w, FiniteWitness w) => Searchable (AnyWitness w) where
    search = finiteSearch

instance (TestEquality w, FiniteWitness w) => Finite (AnyWitness w) where
    assemble ::
           forall b f. Applicative f
        => (AnyWitness w -> f b)
        -> f (AnyWitness w -> b)
    assemble afb =
        fmap (\(MkAllF wtcb) (MkAnyWitness wt) -> getConst $ wtcb wt) $
        assembleWitnessF $ \wt -> fmap Const $ afb $ MkAnyWitness wt
    allValues = getConst $ assembleWitnessF $ \wt -> Const [MkAnyWitness wt]

allWitnesses :: FiniteWitness w => [AnyWitness w]
allWitnesses = getConst $ assembleWitnessF $ \wt -> Const [MkAnyWitness wt]

instance (FiniteWitness w, AllWitnessConstraint Show w, WitnessConstraint Show w) => Show (All w) where
    show (MkAll wtt) = let
        showItem :: AnyWitness w -> String
        showItem (MkAnyWitness wt) =
            showAllWitness wt ++
            " -> " ++
            case witnessConstraint @_ @Show wt of
                Dict -> show (wtt wt)
        in "{" ++ intercalate "," (fmap showItem allWitnesses) ++ "}"

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

instance (Show (p t), Show (q t)) => Show (EitherWitness p q t) where
    show (LeftWitness rt) = show rt
    show (RightWitness rt) = show rt

instance (AllWitnessConstraint Show p, AllWitnessConstraint Show q) =>
         AllWitnessConstraint Show (EitherWitness p q) where
    allWitnessConstraint :: forall t. Dict (Show (EitherWitness p q t))
    allWitnessConstraint =
        case allWitnessConstraint @_ @_ @Show @p @t of
            Dict ->
                case allWitnessConstraint @_ @_ @Show @q @t of
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

data ListType' (f :: k -> *) (lt :: [k]) where
    NilListType' :: ListType' f '[]
    ConsListType' :: f a -> ListType' f lt -> ListType' f (a : lt)

class KnownList (t :: [k]) where
    listType :: ListType' Proxy t

instance KnownList '[] where
    listType = NilListType'

instance KnownList lt => KnownList (a : lt) where
    listType = ConsListType' Proxy listType

data ListThingWitness (kk :: [k]) (t :: k) where
    FirstListThingWitness :: ListThingWitness (t : tt) t
    RestListThingWitness :: ListThingWitness aa t -> ListThingWitness (a : aa) t

instance TestEquality (ListThingWitness tt) where
    testEquality FirstListThingWitness FirstListThingWitness = Just Refl
    testEquality (RestListThingWitness lt1) (RestListThingWitness lt2) = do
        Refl <- testEquality lt1 lt2
        return Refl
    testEquality _ _ = Nothing

instance Searchable (ListThingWitness '[] t) where
    search = finiteSearch

instance Eq (ListThingWitness tt t) where
    lt1 == lt2 = isJust $ testEquality lt1 lt2

instance Countable (ListThingWitness '[] t) where
    countPrevious = finiteCountPrevious
    countMaybeNext = finiteCountMaybeNext

instance Finite (ListThingWitness '[] t) where
    allValues = []

instance Empty (ListThingWitness '[] t) where
    never lt = case lt of {}

class KnownList (FiniteConsWitness sel) =>
      IsFiniteConsWitness (sel :: k -> *) where
    type FiniteConsWitness sel :: [k]
    toLTW :: forall t. sel t -> ListThingWitness (FiniteConsWitness sel) t
    fromLTW :: forall t. ListThingWitness (FiniteConsWitness sel) t -> sel t

instance KnownList edits => IsFiniteConsWitness (ListThingWitness edits) where
    type FiniteConsWitness (ListThingWitness edits) = edits
    toLTW = id
    fromLTW = id

instance IsFiniteConsWitness EmptyWitness where
    type FiniteConsWitness EmptyWitness = '[]
    toLTW wit = never wit
    fromLTW lt = never lt

instance IsFiniteConsWitness lt => IsFiniteConsWitness (ConsWitness a lt) where
    type FiniteConsWitness (ConsWitness a lt) = a : (FiniteConsWitness lt)
    toLTW FirstWitness = FirstListThingWitness
    toLTW (RestWitness sel) = RestListThingWitness $ toLTW sel
    fromLTW FirstListThingWitness = FirstWitness
    fromLTW (RestListThingWitness lt) = RestWitness $ fromLTW lt
