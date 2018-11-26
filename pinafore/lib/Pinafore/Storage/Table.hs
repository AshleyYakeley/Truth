module Pinafore.Storage.Table
    ( Anchor(..)
    , Predicate(..)
    , Point(..)
    , PinaforeTableRead(..)
    , PinaforeTableEdit(..)
    , pinaforeTablePointObject
    ) where

import Pinafore.Base
import Shapes
import Truth.Core

data PinaforeTableRead t where
    PinaforeTableReadGetPredicate :: Predicate -> Point -> PinaforeTableRead (Maybe Point)
    PinaforeTableReadLookupPredicate :: Predicate -> Point -> PinaforeTableRead (FiniteSet Point)
    PinaforeTableReadGetLiteral :: Point -> PinaforeTableRead (Maybe Literal)

instance Show (PinaforeTableRead t) where
    show (PinaforeTableReadGetPredicate p s) = "get " ++ show p ++ " of " ++ show s
    show (PinaforeTableReadLookupPredicate p v) = "lookup " ++ show p ++ " for " ++ show v
    show (PinaforeTableReadGetLiteral v) = "get literal of " ++ show v

instance WitnessConstraint Show PinaforeTableRead where
    witnessConstraint (PinaforeTableReadGetPredicate _ _) = Dict
    witnessConstraint (PinaforeTableReadLookupPredicate _ _) = Dict
    witnessConstraint (PinaforeTableReadGetLiteral _) = Dict

instance AllWitnessConstraint Show PinaforeTableRead where
    allWitnessConstraint = Dict

data PinaforeTableEdit where
    PinaforeTableEditSetPredicate :: Predicate -> Point -> Maybe Point -> PinaforeTableEdit -- pred subj mval
    PinaforeTableEditSetLiteral :: Point -> Maybe Literal -> PinaforeTableEdit

instance Show PinaforeTableEdit where
    show (PinaforeTableEditSetPredicate p s mv) = "set " ++ show p ++ " of " ++ show s ++ " to " ++ show mv
    show (PinaforeTableEditSetLiteral v ml) = "set literal of " ++ show v ++ " to " ++ show ml

instance SubjectReader PinaforeTableRead where
    type ReaderSubject PinaforeTableRead = ([(Predicate, Point, Point)], [(Point, Literal)])
    subjectToRead (triples, _) (PinaforeTableReadGetPredicate rp rs) =
        listToMaybe $ [v | (p, s, v) <- triples, p == rp && s == rs]
    subjectToRead (triples, _) (PinaforeTableReadLookupPredicate rp rv) =
        MkFiniteSet [s | (p, s, v) <- triples, p == rp, v == rv]
    subjectToRead (_, literals) (PinaforeTableReadGetLiteral rv) = listToMaybe [l | (v, l) <- literals, v == rv]

instance Floating PinaforeTableEdit PinaforeTableEdit

type instance EditReader PinaforeTableEdit = PinaforeTableRead

instance ApplicableEdit PinaforeTableEdit where
    applyEdit (PinaforeTableEditSetPredicate p s mv) _ (PinaforeTableReadGetPredicate p' s')
        | p == p' && s == s' = return mv
    applyEdit (PinaforeTableEditSetPredicate p s mv) mr (PinaforeTableReadLookupPredicate p' v')
        | p == p' = do
            fs <- mr $ PinaforeTableReadLookupPredicate p' v'
            return $
                case mv of
                    Just v
                        | v == v' -> insertSet s fs
                    _ -> deleteSet s fs
    applyEdit (PinaforeTableEditSetLiteral v ml) _mr (PinaforeTableReadGetLiteral v')
        | v == v' = return ml
    applyEdit _ mr rt = mr rt

replaceFirst :: (a -> Maybe (Maybe a, b)) -> [a] -> ([a], Maybe b)
replaceFirst _ [] = ([], Nothing)
replaceFirst f (a:aa)
    | Just (ma, b) <- f a =
        case ma of
            Just a' -> (a' : aa, Just b)
            Nothing -> (aa, Just b)
replaceFirst f (a:aa) =
    case replaceFirst f aa of
        (aa', mb) -> (a : aa', mb)

instance SubjectMapEdit PinaforeTableEdit where
    mapSubjectEdits =
        mapEditToMapEdits $ \edit (oldtriples, oldliterals) ->
            case edit of
                PinaforeTableEditSetPredicate prd s mv -> let
                    match (prd', s', _)
                        | prd' == prd
                        , s == s' = Just (fmap (\v -> (prd, s, v)) mv, ())
                    match _ = Nothing
                    (newtriples', mu) = replaceFirst match oldtriples
                    newtriples =
                        case (mu, mv) of
                            (Nothing, Just v) -> (prd, s, v) : newtriples'
                            _ -> newtriples'
                    in return $ (newtriples, oldliterals)
                PinaforeTableEditSetLiteral v ml -> let
                    match (v', _)
                        | v == v' = Just (fmap (\l -> (v, l)) ml, ())
                    match _ = Nothing
                    (newliterals', mu) = replaceFirst match oldliterals
                    newliterals =
                        case (mu, ml) of
                            (Nothing, Just l) -> (v, l) : newliterals'
                            _ -> newliterals'
                    in return $ (oldtriples, newliterals)

instance InvertibleEdit PinaforeTableEdit where
    invertEdit (PinaforeTableEditSetPredicate p s _) mr = do
        mv <- mr $ PinaforeTableReadGetPredicate p s
        return [PinaforeTableEditSetPredicate p s mv]
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
    LiteralPinaforeTableEditCacheKey :: PinaforeTableEditCacheKey cache (cache (PointCacheKey cache Literal))

instance TestEquality (PinaforeTableEditCacheKey cache) where
    testEquality (PredicatePinaforeTableEditCacheKey p1) (PredicatePinaforeTableEditCacheKey p2)
        | p1 == p2 = Just Refl
    testEquality LiteralPinaforeTableEditCacheKey LiteralPinaforeTableEditCacheKey = Just Refl
    testEquality _ _ = Nothing

instance CacheableEdit PinaforeTableEdit where
    type EditCacheKey cache PinaforeTableEdit = PinaforeTableEditCacheKey cache
    editCacheAdd (PinaforeTableReadGetPredicate p s) mv =
        subcacheModify (PredicatePinaforeTableEditCacheKey p) $
        subcacheModify GetPointCacheKey $ cacheAdd (MkSimpleCacheKey s) mv
    editCacheAdd (PinaforeTableReadLookupPredicate p v) fs =
        subcacheModify (PredicatePinaforeTableEditCacheKey p) $
        subcacheModify LookupPointCacheKey $ cacheAdd (MkSimpleCacheKey v) fs
    editCacheAdd (PinaforeTableReadGetLiteral s) mv =
        subcacheModify LiteralPinaforeTableEditCacheKey $
        subcacheModify GetPointCacheKey $ cacheAdd (MkSimpleCacheKey s) mv
    editCacheLookup (PinaforeTableReadGetPredicate p s) cache = do
        subcache1 <- cacheLookup (PredicatePinaforeTableEditCacheKey p) cache
        subcache2 <- cacheLookup GetPointCacheKey subcache1
        cacheLookup (MkSimpleCacheKey s) subcache2
    editCacheLookup (PinaforeTableReadLookupPredicate p v) cache = do
        subcache1 <- cacheLookup (PredicatePinaforeTableEditCacheKey p) cache
        subcache2 <- cacheLookup LookupPointCacheKey subcache1
        cacheLookup (MkSimpleCacheKey v) subcache2
    editCacheLookup (PinaforeTableReadGetLiteral s) cache = do
        subcache1 <- cacheLookup LiteralPinaforeTableEditCacheKey cache
        subcache2 <- cacheLookup GetPointCacheKey subcache1
        cacheLookup (MkSimpleCacheKey s) subcache2
    editCacheUpdate (PinaforeTableEditSetPredicate p s mv) =
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

pinaforeTablePointObject :: Object PinaforeTableEdit -> Object PinaforePointEdit
pinaforeTablePointObject (MkObject objRun (tableRead :: MutableRead m PinaforeTableRead) tableMPush) = let
    tablePush :: [PinaforeTableEdit] -> m ()
    tablePush edits = pushOrFail "can't push table edit" $ tableMPush edits
    objRead :: MutableRead m PinaforePointRead
    objRead (PinaforePointReadGetPredicate prd subj) = do
        mval <- tableRead $ PinaforeTableReadGetPredicate prd subj
        case mval of
            Just val -> return val
            Nothing -> do
                val <- newPoint
                tablePush [PinaforeTableEditSetPredicate prd subj $ Just val]
                return val
    objRead (PinaforePointReadLookupPredicate prd val) = tableRead $ PinaforeTableReadLookupPredicate prd val
    objRead (PinaforePointReadToLiteral p) = do
        ml <- tableRead $ PinaforeTableReadGetLiteral p
        return $ maybeToKnow ml
    objEdit :: [PinaforePointEdit] -> m (Maybe (m ()))
    objEdit =
        singleAlwaysEdit $ \case
            PinaforePointEditSetPredicate p s kv -> tablePush [PinaforeTableEditSetPredicate p s $ knowToMaybe kv]
            PinaforePointEditSetLiteral p kl -> tablePush [PinaforeTableEditSetLiteral p $ knowToMaybe kl]
    in MkObject {..}
