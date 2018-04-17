module Pinafore.Table
    ( Predicate(..)
    , Point(..)
    , PinaforeTableRead(..)
    , PinaforeTableEdit(..)
    , HasPinaforeTableEdit(..)
    , literalPinaforeLensMorphism
    , predicatePinaforeLensMorphism
    ) where

import Data.Aeson (FromJSON)
import Data.Serialize as Serialize (Serialize(..))
import Data.UUID hiding (fromString, fromText, toText)
import Pinafore.AsText
import Pinafore.Morphism
import Shapes
import Truth.Core
import Truth.Debug.Object

newtype Predicate =
    MkPredicate UUID
    deriving (Eq, FromJSON)

instance Show Predicate where
    show (MkPredicate uuid) = '%' : show uuid

newtype Point =
    MkPoint UUID
    deriving (Eq, Random, FromJSON)

instance Show Point where
    show (MkPoint uuid) = '!' : show uuid

instance Serialize Point where
    put (MkPoint uuid) = Serialize.put (toByteString uuid)
    get = do
        bs <- Serialize.get
        case fromByteString bs of
            Just uuid -> return $ MkPoint uuid
            Nothing -> fail "deserialize bad UUID"

data PinaforeTableRead t where
    PinaforeTableReadGetValue :: Predicate -> Point -> PinaforeTableRead (Maybe Point)
    PinaforeTableReadLookupValue :: Predicate -> Point -> PinaforeTableRead (FiniteSet Point)
    PinaforeTableReadGetLiteral :: Point -> PinaforeTableRead (Maybe Text)
    PinaforeTableReadLookupLiteral :: Text -> PinaforeTableRead (FiniteSet Point)

instance Show (PinaforeTableRead t) where
    show (PinaforeTableReadGetValue p s) = "get " ++ show p ++ " of " ++ show s
    show (PinaforeTableReadLookupValue p v) = "lookup " ++ show p ++ " for " ++ show v
    show (PinaforeTableReadGetLiteral v) = "get literal of " ++ show v
    show (PinaforeTableReadLookupLiteral l) = "lookup literal for " ++ show l

instance WitnessConstraint Show PinaforeTableRead where
    witnessConstraint (PinaforeTableReadGetValue _ _) = Dict
    witnessConstraint (PinaforeTableReadLookupValue _ _) = Dict
    witnessConstraint (PinaforeTableReadGetLiteral _) = Dict
    witnessConstraint (PinaforeTableReadLookupLiteral _) = Dict

instance AllWitnessConstraint Show PinaforeTableRead where
    allWitnessConstraint = Dict

data PinaforeTableEdit where
    PinaforeTableEditSetValue :: Predicate -> Point -> Maybe Point -> PinaforeTableEdit -- pred subj mval
    PinaforeTableEditSetLiteral :: Point -> Maybe Text -> PinaforeTableEdit

instance Show PinaforeTableEdit where
    show (PinaforeTableEditSetValue p s mv) = "set " ++ show p ++ " of " ++ show s ++ " to " ++ show mv
    show (PinaforeTableEditSetLiteral v ml) = "set literal of " ++ show v ++ " to " ++ show ml

instance SubjectReader PinaforeTableRead where
    type ReaderSubject PinaforeTableRead = ([(Predicate, Point, Point)], [(Point, Text)])
    subjectToRead (triples, _) (PinaforeTableReadGetValue rp rs) =
        listToMaybe $ [v | (p, s, v) <- triples, p == rp && s == rs]
    subjectToRead (triples, _) (PinaforeTableReadLookupValue rp rv) =
        MkFiniteSet [s | (p, s, v) <- triples, p == rp, v == rv]
    subjectToRead (_, literals) (PinaforeTableReadGetLiteral rv) = listToMaybe [l | (v, l) <- literals, v == rv]
    subjectToRead (_, literals) (PinaforeTableReadLookupLiteral rl) = MkFiniteSet [v | (v, l) <- literals, l == rl]

instance Floating PinaforeTableEdit PinaforeTableEdit

type instance EditReader PinaforeTableEdit = PinaforeTableRead

instance ApplicableEdit PinaforeTableEdit where
    applyEdit (PinaforeTableEditSetValue p s mv) _ (PinaforeTableReadGetValue p' s')
        | p == p' && s == s' = return mv
    applyEdit (PinaforeTableEditSetValue p s mv) mr (PinaforeTableReadLookupValue p' v')
        | p == p' = do
            fs <- mr $ PinaforeTableReadLookupValue p' v'
            return $
                case mv of
                    Just v
                        | v == v' -> insertSet s fs
                    _ -> deleteSet s fs
    applyEdit (PinaforeTableEditSetLiteral v ml) _mr (PinaforeTableReadGetLiteral v')
        | v == v' = return ml
    applyEdit (PinaforeTableEditSetLiteral v ml) mr (PinaforeTableReadLookupLiteral l') = do
        fv <- mr $ PinaforeTableReadLookupLiteral l'
        return $
            case ml of
                Just l
                    | l == l' -> insertSet v fv
                _ -> deleteSet v fv
    applyEdit _ mr rt = mr rt

instance InvertibleEdit PinaforeTableEdit where
    invertEdit (PinaforeTableEditSetValue p s _) mr = do
        mv <- mr $ PinaforeTableReadGetValue p s
        return [PinaforeTableEditSetValue p s mv]
    invertEdit (PinaforeTableEditSetLiteral v _) mr = do
        ml <- mr $ PinaforeTableReadGetLiteral v
        return [PinaforeTableEditSetLiteral v ml]

data PointCacheKey cache t ct where
    GetPointCacheKey :: PointCacheKey cache t (cache (SimpleCacheKey Point (Maybe t)))
    LookupPointCacheKey :: PointCacheKey cache t (cache (SimpleCacheKey t (FiniteSet Point)))

instance Eq t => TestEquality (PointCacheKey cache t) where
    testEquality GetPointCacheKey GetPointCacheKey = Just Refl
    testEquality LookupPointCacheKey LookupPointCacheKey = Just Refl
    testEquality _ _ = Nothing

data PinaforeTableEditCacheKey cache ct where
    PredicatePinaforeTableEditCacheKey
        :: Predicate -> PinaforeTableEditCacheKey cache (cache (PointCacheKey cache Point))
    LiteralPinaforeTableEditCacheKey :: PinaforeTableEditCacheKey cache (cache (PointCacheKey cache Text))

instance TestEquality (PinaforeTableEditCacheKey cache) where
    testEquality (PredicatePinaforeTableEditCacheKey p1) (PredicatePinaforeTableEditCacheKey p2)
        | p1 == p2 = Just Refl
    testEquality LiteralPinaforeTableEditCacheKey LiteralPinaforeTableEditCacheKey = Just Refl
    testEquality _ _ = Nothing

instance CacheableEdit PinaforeTableEdit where
    type EditCacheKey cache PinaforeTableEdit = PinaforeTableEditCacheKey cache
    editCacheAdd (PinaforeTableReadGetValue p s) mv =
        subcacheModify (PredicatePinaforeTableEditCacheKey p) $
        subcacheModify GetPointCacheKey $ cacheAdd (MkSimpleCacheKey s) mv
    editCacheAdd (PinaforeTableReadLookupValue p v) fs =
        subcacheModify (PredicatePinaforeTableEditCacheKey p) $
        subcacheModify LookupPointCacheKey $ cacheAdd (MkSimpleCacheKey v) fs
    editCacheAdd (PinaforeTableReadGetLiteral s) mv =
        subcacheModify LiteralPinaforeTableEditCacheKey $
        subcacheModify GetPointCacheKey $ cacheAdd (MkSimpleCacheKey s) mv
    editCacheAdd (PinaforeTableReadLookupLiteral v) fs =
        subcacheModify LiteralPinaforeTableEditCacheKey $
        subcacheModify LookupPointCacheKey $ cacheAdd (MkSimpleCacheKey v) fs
    editCacheLookup (PinaforeTableReadGetValue p s) cache = do
        subcache1 <- cacheLookup (PredicatePinaforeTableEditCacheKey p) cache
        subcache2 <- cacheLookup GetPointCacheKey subcache1
        cacheLookup (MkSimpleCacheKey s) subcache2
    editCacheLookup (PinaforeTableReadLookupValue p v) cache = do
        subcache1 <- cacheLookup (PredicatePinaforeTableEditCacheKey p) cache
        subcache2 <- cacheLookup LookupPointCacheKey subcache1
        cacheLookup (MkSimpleCacheKey v) subcache2
    editCacheLookup (PinaforeTableReadGetLiteral s) cache = do
        subcache1 <- cacheLookup LiteralPinaforeTableEditCacheKey cache
        subcache2 <- cacheLookup GetPointCacheKey subcache1
        cacheLookup (MkSimpleCacheKey s) subcache2
    editCacheLookup (PinaforeTableReadLookupLiteral v) cache = do
        subcache1 <- cacheLookup LiteralPinaforeTableEditCacheKey cache
        subcache2 <- cacheLookup LookupPointCacheKey subcache1
        cacheLookup (MkSimpleCacheKey v) subcache2
    editCacheUpdate (PinaforeTableEditSetValue p s mv) =
        subcacheModify (PredicatePinaforeTableEditCacheKey p) $ do
            subcacheModify GetPointCacheKey $ cacheModify (MkSimpleCacheKey s) $ Shapes.put $ Just mv
            subcacheModify LookupPointCacheKey $
                cacheTraverse $ \(MkSimpleCacheKey v') ss' ->
                    return $
                    Just $
                    (if mv == Just v'
                         then insertElement
                         else deleteElement)
                        s
                        ss'
    editCacheUpdate (PinaforeTableEditSetLiteral v mt) =
        subcacheModify LiteralPinaforeTableEditCacheKey $ do
            subcacheModify GetPointCacheKey $ cacheModify (MkSimpleCacheKey v) $ Shapes.put $ Just mt
            subcacheModify LookupPointCacheKey $
                cacheTraverse $ \(MkSimpleCacheKey t') vv' ->
                    return $
                    Just $
                    (if mt == Just t'
                         then insertElement
                         else deleteElement)
                        v
                        vv'

class HasPinaforeTableEdit baseedit where
    pinaforeTableLens :: EditLens baseedit PinaforeTableEdit

instance HasPinaforeTableEdit PinaforeTableEdit where
    pinaforeTableLens = id

literalPinaforeMap ::
       forall val. AsText val
    => AnEditLens IdentityT (ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))) (WholeEdit (Maybe val))
literalPinaforeMap = {-traceArgAThing "literalPinaforeMap" $-} let
    efGet ::
           ReadFunctionT IdentityT (ContextEditReader PinaforeTableEdit (WholeEdit (Maybe Point))) (WholeReader (Maybe val))
    efGet mr ReadWhole =
        lift $ do
            msubj <- mr $ MkTupleEditReader SelectContent ReadWhole
            case msubj of
                Just subj -> do
                    mbs <- mr $ MkTupleEditReader SelectContext $ PinaforeTableReadGetLiteral subj
                    return $ mbs >>= fromText
                Nothing -> return Nothing
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))
        -> MutableRead m (ContextEditReader PinaforeTableEdit (WholeEdit (Maybe Point)))
        -> IdentityT m [WholeEdit (Maybe val)]
    efUpdate (MkTupleEdit SelectContext (PinaforeTableEditSetLiteral s mbs)) mr = do
        msubj <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
        return $
            if Just s == msubj
                then [MkWholeEdit $ mbs >>= fromText]
                else []
    efUpdate (MkTupleEdit SelectContext _) _ = return []
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit Nothing)) _ = return [MkWholeEdit Nothing]
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit (Just subj))) mr = do
        mbs <- lift $ mr $ MkTupleEditReader SelectContext $ PinaforeTableReadGetLiteral subj
        return $ [MkWholeEdit $ mbs >>= fromText]
    elFunction ::
           AnEditFunction IdentityT (ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))) (WholeEdit (Maybe val))
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit (Maybe val)
        -> MutableRead m (ContextEditReader PinaforeTableEdit (WholeEdit (Maybe Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))])
    elPutEdit (MkWholeEdit (fmap toText -> mbs)) mr = do
        msubj <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
        case msubj of
            Just subj -> return $ Just [MkTupleEdit SelectContext $ PinaforeTableEditSetLiteral subj mbs]
            Nothing -> do
                subj <- liftIO randomIO
                return $
                    Just
                        [ MkTupleEdit SelectContent $ MkWholeEdit $ Just subj
                        , MkTupleEdit SelectContext $ PinaforeTableEditSetLiteral subj mbs
                        ]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Maybe val)]
        -> MutableRead m (ContextEditReader PinaforeTableEdit (WholeEdit (Maybe Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))])
    elPutEdits [] _ = return $ Just []
    elPutEdits [edit] mr = elPutEdit edit mr
    elPutEdits (_:edits) mr = elPutEdits edits mr -- just use the last WholeEdit.
    in MkAnEditLens {..}

literalInverseFunction ::
       forall val. AsText val
    => APinaforeFunctionMorphism PinaforeTableEdit IdentityT val (FiniteSet Point)
literalInverseFunction = let
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m PinaforeTableRead
        -> val
        -> IdentityT m (FiniteSet Point)
    pfFuncRead mr val = lift $ mr $ PinaforeTableReadLookupLiteral $ toText val
    pfUpdate ::
           forall m. MonadIO m
        => PinaforeTableEdit
        -> MutableRead m PinaforeTableRead
        -> IdentityT m Bool
    pfUpdate (PinaforeTableEditSetLiteral _ _) _ = return True
    pfUpdate _ _ = return False
    in MkAPinaforeFunctionMorphism {..}

literalPinaforeTableLensMorphism ::
       forall val. AsText val
    => PinaforeLensMorphism PinaforeTableEdit Point val
literalPinaforeTableLensMorphism =
    MkCloseUnlift identityUnlift $ MkAPinaforeLensMorphism literalPinaforeMap literalInverseFunction

literalPinaforeLensMorphism :: (HasPinaforeTableEdit baseedit, AsText val) => PinaforeLensMorphism baseedit Point val
literalPinaforeLensMorphism = mapPinaforeLensMorphismBase pinaforeTableLens literalPinaforeTableLensMorphism

predicatePinaforeMap ::
       Predicate
    -> AnEditLens IdentityT (ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))) (WholeEdit (Maybe Point))
predicatePinaforeMap prd = traceArgAThing "predicatePinaforeMap" $ let
    efGet ::
           ReadFunctionT IdentityT (ContextEditReader PinaforeTableEdit (WholeEdit (Maybe Point))) (WholeReader (Maybe Point))
    efGet mr ReadWhole =
        lift $ do
            msubj <- mr $ MkTupleEditReader SelectContent ReadWhole
            case msubj of
                Just subj -> mr $ MkTupleEditReader SelectContext $ PinaforeTableReadGetValue prd subj
                Nothing -> return Nothing
    efUpdate ::
           forall m. MonadIO m
        => ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))
        -> MutableRead m (ContextEditReader PinaforeTableEdit (WholeEdit (Maybe Point)))
        -> IdentityT m [WholeEdit (Maybe Point)]
    efUpdate (MkTupleEdit SelectContext (PinaforeTableEditSetValue p s mv)) mr
        | p == prd = do
            msubj <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
            return $
                if Just s == msubj
                    then [MkWholeEdit mv]
                    else []
    efUpdate (MkTupleEdit SelectContext _) _ = return []
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit Nothing)) _ = return [MkWholeEdit Nothing]
    efUpdate (MkTupleEdit SelectContent (MkWholeEdit (Just subj))) mr = do
        mval <- lift $ mr $ MkTupleEditReader SelectContext $ PinaforeTableReadGetValue prd subj
        return [MkWholeEdit mval]
    elFunction ::
           AnEditFunction IdentityT (ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))) (WholeEdit (Maybe Point))
    elFunction = MkAnEditFunction {..}
    elPutEdit ::
           forall m. MonadIO m
        => WholeEdit (Maybe Point)
        -> MutableRead m (ContextEditReader PinaforeTableEdit (WholeEdit (Maybe Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))])
    elPutEdit (MkWholeEdit mv) mr = do
        msubj <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
        case msubj of
            Just subj -> return $ Just [MkTupleEdit SelectContext $ PinaforeTableEditSetValue prd subj mv]
            Nothing -> do
                subj <- liftIO randomIO
                return $
                    Just
                        [ MkTupleEdit SelectContent $ MkWholeEdit $ Just subj
                        , MkTupleEdit SelectContext $ PinaforeTableEditSetValue prd subj mv
                        ]
    elPutEdits ::
           forall m. MonadIO m
        => [WholeEdit (Maybe Point)]
        -> MutableRead m (ContextEditReader PinaforeTableEdit (WholeEdit (Maybe Point)))
        -> IdentityT m (Maybe [ContextEdit PinaforeTableEdit (WholeEdit (Maybe Point))])
    elPutEdits [] _ = return $ Just []
    elPutEdits [edit] mr = elPutEdit edit mr
    elPutEdits (_:edits) mr = elPutEdits edits mr -- just use the last WholeEdit.
    in MkAnEditLens {..}

predicateInverseFunction :: Predicate -> APinaforeFunctionMorphism PinaforeTableEdit IdentityT Point (FiniteSet Point)
predicateInverseFunction prd = let
    pfFuncRead ::
           forall m. MonadIO m
        => MutableRead m PinaforeTableRead
        -> Point
        -> IdentityT m (FiniteSet Point)
    pfFuncRead mr val = lift $ mr $ PinaforeTableReadLookupValue prd val
    pfUpdate ::
           forall m. MonadIO m
        => PinaforeTableEdit
        -> MutableRead m PinaforeTableRead
        -> IdentityT m Bool
    pfUpdate (PinaforeTableEditSetValue p _ _) _
        | p == prd = return True
    pfUpdate _ _ = return False
    in MkAPinaforeFunctionMorphism {..}

predicatePinaforeTableLensMorphism :: Predicate -> PinaforeLensMorphism PinaforeTableEdit Point Point
predicatePinaforeTableLensMorphism prd =
    MkCloseUnlift identityUnlift $ MkAPinaforeLensMorphism (predicatePinaforeMap prd) (predicateInverseFunction prd)

predicatePinaforeLensMorphism :: HasPinaforeTableEdit baseedit => Predicate -> PinaforeLensMorphism baseedit Point Point
predicatePinaforeLensMorphism prd =
    mapPinaforeLensMorphismBase pinaforeTableLens $ predicatePinaforeTableLensMorphism prd
