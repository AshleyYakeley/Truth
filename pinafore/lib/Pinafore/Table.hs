module Pinafore.Table
    ( Predicate(..)
    , Point(..)
    , PinaforeTableRead(..)
    , PinaforeTableEdit(..)
    , pinaforeTablePointObject
    ) where

import Pinafore.Literal
import Pinafore.Point
import Shapes
import Text.Read
import Truth.Core

data PinaforeTableRead t where
    PinaforeTableReadGetPredicate :: Predicate -> Point -> PinaforeTableRead (Maybe Point)
    PinaforeTableReadLookupPredicate :: Predicate -> Point -> PinaforeTableRead (FiniteSet Point)
    PinaforeTableReadGetLiteral :: Point -> PinaforeTableRead (Maybe Literal)
    PinaforeTableReadLookupLiteral :: Literal -> PinaforeTableRead (FiniteSet Point)

instance Show (PinaforeTableRead t) where
    show (PinaforeTableReadGetPredicate p s) = "get " ++ show p ++ " of " ++ show s
    show (PinaforeTableReadLookupPredicate p v) = "lookup " ++ show p ++ " for " ++ show v
    show (PinaforeTableReadGetLiteral v) = "get literal of " ++ show v
    show (PinaforeTableReadLookupLiteral l) = "lookup literal for " ++ show l

instance WitnessConstraint Show PinaforeTableRead where
    witnessConstraint (PinaforeTableReadGetPredicate _ _) = Dict
    witnessConstraint (PinaforeTableReadLookupPredicate _ _) = Dict
    witnessConstraint (PinaforeTableReadGetLiteral _) = Dict
    witnessConstraint (PinaforeTableReadLookupLiteral _) = Dict

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
    subjectToRead (_, literals) (PinaforeTableReadLookupLiteral rl) = MkFiniteSet [v | (v, l) <- literals, l == rl]

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
    applyEdit (PinaforeTableEditSetLiteral v ml) mr (PinaforeTableReadLookupLiteral l') = do
        fv <- mr $ PinaforeTableReadLookupLiteral l'
        return $
            case ml of
                Just l
                    | l == l' -> insertSet v fv
                _ -> deleteSet v fv
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
    editCacheAdd (PinaforeTableReadLookupLiteral v) fs =
        subcacheModify LiteralPinaforeTableEditCacheKey $
        subcacheModify LookupPointCacheKey $ cacheAdd (MkSimpleCacheKey v) fs
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
    editCacheLookup (PinaforeTableReadLookupLiteral v) cache = do
        subcache1 <- cacheLookup LiteralPinaforeTableEditCacheKey cache
        subcache2 <- cacheLookup LookupPointCacheKey subcache1
        cacheLookup (MkSimpleCacheKey v) subcache2
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

---
predfst :: Predicate
predfst = MkPredicate $ read "9fa17b88-89f3-4baf-b4b3-fdb7280a0020"

predsnd :: Predicate
predsnd = MkPredicate $ read "3c667db3-c3a5-4ef9-9b15-6cad178c50c7"

predinl :: Predicate
predinl = MkPredicate $ read "ffb8fee1-6971-46c0-9954-62c2ec53e98a"

predinr :: Predicate
predinr = MkPredicate $ read "bbc7a8ca-17e1-4d42-9230-e6b889dea2e5"

unitPoint :: Point
unitPoint = MkPoint $ read "644eaa9b-0c57-4c5c-9606-e5303fda86f9"

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
        return $ ml >>= fromLiteral
    objRead (PinaforePointReadFromLiteral t) = do
        let l = toLiteral t
        s <- tableRead $ PinaforeTableReadLookupLiteral l
        case getSingle s of
            Just p -> return p
            Nothing -> do
                p <- newPoint -- could hash l instead
                tablePush [PinaforeTableEditSetLiteral p $ Just l]
                return p
    objRead PinaforePointReadUnit = return unitPoint
    objRead (PinaforePointReadToPair p) =
        getComposeM $ do
            p1 <- MkComposeM $ tableRead $ PinaforeTableReadGetPredicate predfst p
            p2 <- MkComposeM $ tableRead $ PinaforeTableReadGetPredicate predsnd p
            return (p1, p2)
    objRead (PinaforePointReadFromPair (p1, p2)) = do
        s1 <- tableRead $ PinaforeTableReadLookupPredicate predfst p1
        s2 <- tableRead $ PinaforeTableReadLookupPredicate predsnd p2
        case getSingle $ s1 /\ s2 of
            Just p -> return p
            Nothing -> do
                p <- newPoint
                tablePush
                    [ PinaforeTableEditSetPredicate predfst p $ Just p1
                    , PinaforeTableEditSetPredicate predsnd p $ Just p2
                    ]
                return p
    objRead (PinaforePointReadToEither p) = do
        sl <- tableRead $ PinaforeTableReadLookupPredicate predinl p
        case getSingle sl of
            Just l -> return $ Just $ Left l
            Nothing -> do
                sr <- tableRead $ PinaforeTableReadLookupPredicate predinr p
                case getSingle sr of
                    Just r -> return $ Just $ Right r
                    Nothing -> return Nothing
    objRead (PinaforePointReadFromEither (Left p)) = objRead $ PinaforePointReadGetPredicate predinl p
    objRead (PinaforePointReadFromEither (Right p)) = objRead $ PinaforePointReadGetPredicate predinr p
    objEdit :: [PinaforePointEdit] -> m (Maybe (m ()))
    objEdit =
        singleAlwaysEdit $ \(PinaforePointEditSetPredicate p s v) ->
            tablePush [PinaforeTableEditSetPredicate p s $ Just v]
    in MkObject {..}
